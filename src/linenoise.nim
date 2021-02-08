import os
import terminal
import termios
import posix

import strutils
import sequtils

const LINENOISE_DEFAULT_HISTORY_MAX_LEN = 100
const LINENOISE_MAX_LINE = 4096
const unsupported_term = ["dumb", "cons25", "emacs"]

# TODO: standardize variable names
# TODO: rename all "file descriptor" variable names to "file"
# TODO: make this more idiomatic

# ANCHOR shims/extensions
template inBounds[T](n: int, arr: openArray[T]): bool =
  n in arr.low..arr.high

# ANCHOR struct types
type
  LinenoiseState = object
    ifd: File         #  Terminal stdin file.
    ofd: File         #  Terminal stdout file.
    buf: string       #  Edited line buffer.
    prompt: string    #  Prompt to display.
    pos: int          #  Current cursor position.
    oldpos: int       #  Previous refresh cursor position.
    cols: int         #  Number of columns in terminal.
    maxrows: int      #  Maximum num of rows used so far (multiline mode)
    historyIndex: int #  The history index we are currently editing.
    truebuf: string   #  Buffer, not modified by history

  KeyAction = enum
    KEY_NULL = 0    #  NULL
    CTRL_A = 1      #  Ctrl+a
    CTRL_B = 2      #  Ctrl-b
    CTRL_C = 3      #  Ctrl-c
    CTRL_D = 4      #  Ctrl-d
    CTRL_E = 5      #  Ctrl-e
    CTRL_F = 6      #  Ctrl-f
    CTRL_H = 8      #  Ctrl-h
    TAB = 9         #  Tab
    CTRL_K = 11     #  Ctrl+k
    CTRL_L = 12     #  Ctrl+l
    ENTER = 13      #  Enter
    CTRL_N = 14     #  Ctrl-n
    CTRL_P = 16     #  Ctrl-p
    CTRL_T = 20     #  Ctrl-t
    CTRL_U = 21     #  Ctrl+u
    CTRL_W = 23     #  Ctrl+w
    ESC = 27        #  Escape
    BACKSPACE = 127 #  Backspace

  Completions = seq[string]

# ANCHOR error types
type
  NotATypewriter = object of CatchableError

# ANCHOR callback types
type
  CompletionCallback = proc(buf: string): Completions
  HintsCallback = proc(buf: string): (string, seq[int])

var completionCallback: CompletionCallback
var hintsCallback: HintsCallback

# ANCHOR using statements
using
  ls: ref LinenoiseState

# ANCHOR forward declarations
proc refreshLine(ls)
proc historyAdd*(line: string): int

var
  origTermios: Termios      #  In order to restore at exit.
  maskMode* = false         ##  Show "***" instead of input. For passwords.
  rawMode = false           #  For atexit() function to check if restore is needed
  multiLineMode* = false    ##  Multi line mode. Default is single line.
  atexit_registered = false #  Register atexit just 1 time.
  history_max_len = LINENOISE_DEFAULT_HISTORY_MAX_LEN
  history: seq[string]

const isDebugging = false
when isDebugging:
  var lnDebugFile: File
# Debugging template.
template lnDebug(args: varargs[string, `$`]) {.dirty.} =
  when isDebugging:
    if lnDebugFile == nil:
      lnDebugFile = open("/tmp/lndebug.txt", fmReadWriteExisting)
      lnDebugFile.write(
        "[$# $# $#] p: $#, rows: $#, rpos: $#, max: $#, oldmax: $#\n".format(
          ls[].len, ls[].pos, ls[].oldpos, plen, rows, rpos,
          ls[].maxrows, old_rows
        )
      )

    if args.len > 1:
      lnDebugFile.write(", " & args[0].format(args[1..^1]))
    else:
      lnDebugFile.write(args[0])
    lnDebugFile.flushFile()

proc maskModeEnable* {.deprecated.} =
  ## Enable "mask mode". When it is enabled, instead of the input that
  ## the user is typing, the terminal will just display a corresponding
  ## number of asterisks, like "****". This is useful for passwords and other
  ## secrets that should not be displayed.
  maskMode = true

proc maskModeDisable* {.deprecated.} =
  ## Disable mask mode.
  maskMode = false

proc setMultiLine*(ml: bool) {.deprecated.} =
  ## Set if to use or not the multi line mode.
  multiLineMode = ml


proc isUnsupportedTerm: bool =
  ## Return true if the terminal name is in the list of terminals we know are
  ## not able to understand basic escape sequences.
  let term: string = getEnv("TERM")
  if term == "":
    return false
  if term in unsupported_term:
    return true
  return false

proc enableRawMode(fd: int) =
  # NOTE: uses file descriptor, not file
  var raw: Termios

  if not isAtty(stdin):
    raise newException(NotATypewriter, "")
  if not atexit_registered:
    # atexit(atExit) # TODO: reintroduce this
    atexit_registered = true

  if (tcGetAttr(fd.cint, addr origTermios) == -1):
    raise newException(NotATypewriter, "")

  raw = origTermios # modify the original mode

  # input modes: no break, no CR to NL, no parity check, no strip char,
  # no start/stop output control.
  raw.c_iflag = raw.c_iflag and not (BRKINT or ICRNL or INPCK or ISTRIP or IXON)
  # output modes - disable post processing
  raw.c_oflag = raw.c_oflag and not OPOST
  # control modes - set 8 bit chars
  raw.c_cflag = raw.c_cflag or CS8
  # local modes - echoing off, canonical off, no extended functions,
  # no signal chars (^Z,^C)
  raw.c_lflag = raw.c_lflag and not(ECHO or ICANON or IEXTEN or ISIG)
  # control chars - set return condition: min number of bytes and timer.
  # We want read to return every single byte, without timeout.
  raw.c_cc[VMIN] = 1.cuchar
  raw.c_cc[VTIME] = 0.cuchar # 1 byte, no timer

  # put terminal in raw mode after flushing
  if (tcSetAttr(fd.cint, TCSAFLUSH, addr raw) < 0):
    raise newException(NotATypewriter, "")
  rawMode = true

proc disableRawMode(fd: int) =
  # NOTE: uses file descriptor, not file
  # Don't even check the return value as it's too late.
  if rawMode and tcSetAttr(fd.cint, TCSAFLUSH, addr origTermios) != -1:
    rawMode = false

proc getCursorPosition(inFile, outFile: File): int =
  ## Use the ESC [6n escape sequence to query the horizontal cursor position
  ## and return it. On error -1 is returned, on success the position of the
  ## cursor.
  var buf = newString(8)
  var i: int = 0

  # Report cursor location
  if outFile.writeChars("\e[6n", 0, 4) != 4: return -1

  # Read the response: ESC [ rows  cols R
  while (i < sizeof(buf)-1):
    if (inFile.readChars(buf, i, 1) != 1):
      break
    if (buf[i] == 'R'):
      break
    inc i

  # Parse it.
  if (buf[0] != ESC.char or buf[1] != '['):
    return -1
  buf = buf.strip(chars = {'\0'})
  let rowcol = buf[2..^2].split("").mapIt(it.parseInt)
  return rowcol[1]

proc getColumns(inFile, outFile: File): int =
  ## Try to get the number of columns in the current terminal, or assume 80
  ## if it fails.
  template failed = return 80

  var ws: IOctl_WinSize

  if (ioctl(1, TIOCGWINSZ, addr ws) == -1 or ws.ws_col == 0):
    #  ioctl() failed. Try to query the terminal itself.
    var start, cols: int

    #  Get the initial position so we can restore it later.
    start = getCursorPosition(inFile, outFile)
    if (start == -1): failed

    #  Go to right margin and get position.
    if (outFile.writeChars("\e[999C", 0, 6) != 6): failed
    cols = getCursorPosition(inFile, outFile)
    if (cols == -1): failed

    #  Restore position.
    if (cols > start):
      var sq = "\e[$#D" % $(cols-start)
      if (outFile.writeChars(sq, 0, sq.len) == -1):
        #  Can't recover...
        discard

    return cols
  else:
    return ws.ws_col.int


template clearScreen* =
  ## Clear the screen. Used to handle ctrl+l
  stdout.write("\e[H\e[2J")

template beep* =
  ## Beep, used for completion when there is nothing to complete or when all
  ## the choices were already shown.
  stderr.write("\x7")
  stderr.flushFile()


# ================================ Completion ================================ #

proc completeLine(ls): char =
  ## This is an helper function for edit() and is called when the
  ## user types the <tab> key in order to complete the string currently in the
  ## input.
  ##
  ## The state of the editing is encapsulated into the pointed state
  ## structure as described in the structure definition.

  var
    nread: int
    s: string = "\0"

  var lc = completionCallback(ls[].buf)
  if lc.len == 0:
    beep()
  else:
    var
      stop = false
      i: int = 0
      saved = ls[]

    while not stop:
      # Show completion or original buffer
      if i.inBounds(lc):
        ls[].pos = lc[i].len
        ls[].buf = lc[i]
        ls[].truebuf = lc[i]
        refreshLine(ls)
      else:
        refreshLine(ls)

      nread = ls[].ifd.readChars(s, 0, 1)
      if nread <= 0:
        raise newException(ValueError, "") # TODO: better exceptions

      case s[0]
      of TAB.char:
        i = (i+1) mod lc.len
        if (i == lc.len): beep()
      of ESC.char:
        # Re-show original buffer
        ls[].pos = saved.pos
        ls[].buf = saved.buf
        if i < lc.len: refreshLine(ls)
        stop = true
      else:
        # Update buffer and return
        ls[].buf = lc[i]
        ls[].pos = lc[i].len

        stop = true

  lnDebug("ended", "buf=" & ls[].buf, "lastChar=" & $s[0].int)
  return s[0] # Return last read character


proc setCompletionCallback*(fn: CompletionCallback) =
  ## Register a callback function to be called for tab-completion.
  completionCallback = fn

proc setHintsCallback*(fn: HintsCallback) =
  ## Register a hits function to be called to show hits to the user at the
  ## right of the prompt.
  hintsCallback = fn

proc addCompletion*(lc: var Completions, str: string) {.deprecated.} =
  ## This function is used by the callback function registered by the user
  ## in order to add completion options given the input string when the
  ## user typed <tab>. See the example.c source code for a very easy to
  ## understand example.
  lc.add str

# =============================== Line editing =============================== #

# No need for this, this isn't C
# We define a very simple "append buffer" structure, that is an heap
# allocated string where we can append to. This is useful in order to
# write all the escape sequences in a buffer and flush them to the standard
# output in a single call, to avoid flickering effects.
# type AppendBuffer = ref string

# proc newAppendBuffer: AppendBuffer =

# proc add(ab: AppendBuffer, s: string) =

# No need for this anymore, let Nim's GC handle this
# static void abFree(struct abuf *ab) {}

proc refreshShowHints(toWrite: var string; ls; plen: int) =
  ## Helper of refreshSingleLine() and refreshMultiLine() to show hints
  ## to the right of the prompt.
  if hintsCallback != nil and (plen+ls[].buf.len) < ls[].cols:
    var
      (hint, attributes) = hintsCallback(ls[].buf)
    if (hint != ""):
      toWrite.add:
        if attributes.len > 0:
          "\e[$#;49m" % attributes.join(";")
        else:
          ""
      toWrite.add(hint)
      if attributes.len > 0:
        toWrite.add("\e[0m")

proc refreshSingleLine(ls) =
  ## Single line low level line refresh.
  ##
  ## Rewrite the currently edited line accordingly to the buffer content,
  ## cursor position, and number of columns of the terminal.

  # cut out text when past width of terminal
  var buf = ls[].buf
  if (ls[].prompt.len+buf.len) >= ls[].cols:
    buf = buf[(ls[].prompt.len+ls[].buf.len) - ls[].cols + 1..^1]
    # dec ls[].len
    # dec ls[].pos

  var toWrite = ""
  # Cursor to left edge
  toWrite.add("\r")
  # Write the prompt and the current buffer content
  toWrite.add(ls[].prompt)
  if maskMode:
    toWrite.add("*".repeat(buf.len))
  else:
    toWrite.add(buf)
  # Show hints if any.
  refreshShowHints(toWrite, ls, ls[].prompt.len)
  # Erase to right
  toWrite.add("\e[0K")
  # Move cursor to original position.
  toWrite.add("\r\e[$#C" % $(ls[].pos+ls[].prompt.len))
  ls[].ofd.write(toWrite)

proc refreshMultiLine(ls) =
  ## Multi line low level line refresh.
  ##
  ## Rewrite the currently edited line accordingly to the buffer content,
  ## cursor position, and number of columns of the terminal.
  # FIXME: doesn't work properly
  var
    plen: int = ls[].prompt.len
    rows: int = (plen + ls[].buf.len + ls[].cols - 1) div ls[].cols # rows used by current buf.
    rpos: int = (plen + ls[].oldpos + ls[].cols) div ls[].cols # cursor relative row.
    rpos2: int # rpos after refresh.
    col: int   # colum position, zero-based.
    old_rows: int = ls[].maxrows
    fd = ls[].ofd
    j: int

  # Update maxrows if needed.
  if (rows > ls[].maxrows):
    ls[].maxrows = rows

  # First step: clear all the lines used before. To do so start by
  # going to the last row.
  var s = ""
  if (old_rows-rpos > 0):
    lnDebug("go down $#", old_rows-rpos)
    s.add("\e[$#B" % $(old_rows-rpos))

  # Now for every row clear it, go up.
  for j in 0..<old_rows-1: # -1 is intentional
    lnDebug("clear+up")
    s.add("\r\e[0K\e[1A")

  # Clean the top line.
  lnDebug("clear")
  s.add("\r\e[0K")

  # Write the prompt and the current buffer content
  s.add(ls[].prompt)
  if (maskMode):
    s.add("*".repeat(ls[].buf.len))
  else:
    s.add(ls[].buf)

  # Show hits if any.
  refreshShowHints(s, ls, plen)

  # If we are at the very end of the screen with our prompt, we need to
  # emit a newline and move the prompt to the first column.
  if (ls[].pos != 0 and ls[].pos == ls[].buf.len and
      (ls[].pos + plen) mod ls[].cols == 0):
    lnDebug("<newline>")
    s.add("\n\r")
    inc rows
    if (rows > ls[].maxrows):
      ls[].maxrows = rows

  # Move cursor to right position.
  rpos2 = (plen + ls[].pos + ls[].cols) div ls[].cols # current cursor relative row.
  lnDebug("rpos2 $#", rpos2)

  # Go up till we reach the expected positon.
  if (rows - rpos2 > 0):
    lnDebug("go-up $#", rows-rpos2)
    s.add("\e[$#A" % $(rows-rpos2))

  # Set column.
  col = (plen + ls[].pos) mod ls[].cols
  lnDebug("set col $#", 1+col)
  if (col != 0):
    s.add("\r\e[$#C" % $col)
  else:
    s.add("\r")

  lnDebug("\n")
  ls[].oldpos = ls[].pos

  fd.write(s)

proc refreshLine(ls) =
  ## Calls the two low level functions refreshSingleLine() or
  ## refreshMultiLine() according to the selected mode.
  if multiLineMode:
    refreshMultiLine(ls)
  else:
    refreshSingleLine(ls)

proc editInsert*(ls; c: char) =
  ## Insert the character 'c' at cursor current position.
  if (ls[].buf.len == ls[].pos):
    ls[].buf.add(c)
    inc ls[].pos
    if (not multiLineMode and ls[].prompt.len+ls[].buf.len < ls[].cols and
        hintsCallback == nil):
      # Avoid a full update of the line in the
      # trivial case.
      let d = if maskMode: '*' else: c
      ls[].ofd.write(d)
    else:
      refreshLine(ls)
  else:
    ls[].buf[ls[].pos+1..^1] = ls[].buf[ls[].pos..^1]
    ls[].buf[ls[].pos] = c
    inc ls[].pos
    refreshLine(ls)

proc editMoveLeft*(ls) =
  ## Move cursor on the left.
  if (ls[].pos > 0):
    dec ls[].pos
    refreshLine(ls)

proc editMoveRight*(ls) =
  ## Move cursor on the right.
  if (ls[].pos != ls[].buf.len):
    inc ls[].pos
    refreshLine(ls)

proc editMoveHome*(ls) =
  ## Move cursor to the start of the line.
  if (ls[].pos != 0):
    ls[].pos = 0
    refreshLine(ls)

proc editMoveEnd*(ls) =
  ## Move cursor to the end of the line.
  if (ls[].pos != ls[].buf.len):
    ls[].pos = ls[].buf.len
    refreshLine(ls)

const LINENOISE_HISTORY_NEXT = 0
const LINENOISE_HISTORY_PREV = 1
proc editHistoryNext*(ls; dir: int) =
  ## Substitute the currently edited line with the next or previous history
  ## entry as specified by 'dir'.
  if history.len >= 1:
    # Update the current history entry before to
    # overwrite it with the next one.
    let history_buf = history & ls[].truebuf

    # Show the new entry
    ls[].historyIndex += (if dir == LINENOISE_HISTORY_PREV: 1 else: -1)

    if (ls[].historyIndex < 0):
      ls[].historyIndex = 0
      return
    elif (ls[].historyIndex > history_buf.high):
      ls[].historyIndex = history_buf.high
      return
    ls[].buf = history_buf[history_buf.high - ls[].historyIndex]
    ls[].pos = ls[].buf.len
    refreshLine(ls)

proc editDelete*(ls) =
  ## Delete the character at the right of the cursor without altering the cursor
  ## position. Basically this is what happens with the "Delete" keyboard key.
  if (ls[].buf.len > 0 and ls[].pos < ls[].buf.len):
    ls[].buf[ls[].pos..^1] = ls[].buf[ls[].pos+1..^1]
    refreshLine(ls)

proc editBackspace(ls) =
  ## Backspace implementation.
  if (ls[].pos > 0 and ls[].buf.len > 0):
    ls[].buf[ls[].pos-1..^1] = ls[].buf[ls[].pos..^1]
    dec ls[].pos
    refreshLine(ls)

proc editDeletePrevWord(ls) =
  ## Delete the previous word, maintaining the cursor at the start of the
  ## current word.
  var old_pos = ls[].pos

  while (ls[].pos > 0 and ls[].buf[ls[].pos-1] == ' '):
    dec ls[].pos
  while (ls[].pos > 0 and ls[].buf[ls[].pos-1] != ' '):
    dec ls[].pos

  ls[].buf[ls[].pos..^1] = ls[].buf[old_pos..^1]
  refreshLine(ls)

proc edit(inFile: File, outFile: File, prompt: string): string =
  ## This function is the core of the line editing capability of linenoise.
  ## It expects 'fd' to be already in "raw mode" so that every key pressed
  ## will be returned ASAP to read().
  ##
  ## The resulting string is put into 'buf' when the user type enter, or
  ## when ctrl+d is typed.
  ##
  ## The function returns the current buffer.
  var ls: ref LinenoiseState
  new ls

  # Populate the linenoise state that we pass to functions implementing
  # specific editing functionalities.
  ls[].ifd = inFile
  ls[].ofd = outFile
  ls[].buf = "" # Buffer starts empty.
  ls[].prompt = prompt
  ls[].oldpos = 0
  ls[].pos = 0
  ls[].cols = getColumns(inFile, outFile)
  ls[].maxrows = 0
  ls[].historyIndex = 0

  # The latest history entry is always our current buffer, that
  # initially is just an empty string.
  # discard historyAdd("")

  ls[].ofd.write(prompt)
  while true:
    var c: char
    var sq: array[3, char]

    try:
      c = ls[].ifd.readChar()
    except:
      return ls[].buf

    lnDebug("c was read into", "c=" & c)

    # Only autocomplete when the callback is set. It returns < 0 when
    # there was an error reading from fd. Otherwise it will return the
    # character that should be handled next.
    if (c == '\t' and completionCallback != nil):
      c = completeLine(ls)
      # Read next character when 0
      if (c == '\0'): continue
      # Also continue if escape was pressed
      if (c == ESC.char): continue

    lnDebug("entering main case statement", "buf=" & $ls[].buf)

    case c
    of ENTER.char:
      history.add(ls[].buf)
      if (multiLineMode):
        editMoveEnd(ls)
      if (hintsCallback != nil):
        # Force a refresh without hints to leave the previous
        # line as the user typed it after a newline.
        let hc = hintsCallback
        hintsCallback = nil
        refreshLine(ls)
        hintsCallback = hc
      result = ls[].buf
      ls[].buf = ""
      ls[].truebuf = ""
      return result
    of CTRL_C.char:
      quit() # TODO: emit signal instead?
    of {BACKSPACE.char, CTRL_H.char}:
      editBackspace(ls)
    of CTRL_D.char: # remove char at right of cursor, or if the
                    # line is empty, act as end-of-file.
      if (ls.buf.len > 0):
        editDelete(ls)
      else:
        history.add(ls[].buf)
        ls[].truebuf = ""
        return ls[].buf
    of CTRL_T.char: # swaps current character with previous.
      if (ls.pos > 0 and ls.pos < ls.buf.len):
        let aux = ls[].buf[ls.pos-1]
        ls[].buf[ls.pos-1] = ls[].buf[ls.pos]
        ls[].buf[ls.pos] = aux
        if (ls.pos != ls.buf.len-1):
          inc ls.pos
        refreshLine(ls)
    of CTRL_B.char:
      editMoveLeft(ls)
    of CTRL_F.char:
      editMoveRight(ls)
    of CTRL_P.char:
      editHistoryNext(ls, LINENOISE_HISTORY_PREV)
    of CTRL_N.char:
      editHistoryNext(ls, LINENOISE_HISTORY_NEXT)
    of ESC.char: # escape sequence
        # Read the next two bytes representing the escape sequence.
        # Use two calls to handle slow terminals returning the two
        # chars at different times.
      if (ls.ifd.readChars(sq, 0, 1) == -1): break
      if (ls.ifd.readChars(sq, 1, 1) == -1): break

      # ESC [ sequences.
      if (sq[0] == '['):
        if (sq[1] >= '0' and sq[1] <= '9'):
          # Extended escape, read additional byte.
          if (ls.ifd.readChars(sq, 2, 1) == -1): break
          if (sq[2] == '~'):
            case sq[1]
            of '3': # Delete key.
              editDelete(ls)
            else:
              discard
        else:
          lnDebug("in directions, history = " & $(history & ls[].truebuf))
          case sq[1]
          of 'A': # Up
            editHistoryNext(ls, LINENOISE_HISTORY_PREV)
          of 'B': # Down
            editHistoryNext(ls, LINENOISE_HISTORY_NEXT)
          of 'C': # Right
            editMoveRight(ls)
          of 'D': # Left
            editMoveLeft(ls)
          of 'H': # Home
            editMoveHome(ls)
          of 'F': # End
            editMoveEnd(ls)
          else:
            discard

      # ESC O sequences.
      elif (sq[0] == 'O'):
        case(sq[1])
        of 'H': # Home
          editMoveHome(ls)
        of 'F': # End
          editMoveEnd(ls)
        else:
          discard
    of CTRL_U.char: # delete the whole line.
      ls[].buf = ""
      ls.pos = 0
      refreshLine(ls)
    of CTRL_K.char: # delete from current to end of line.
      ls[].buf = ls[].buf[0..<ls.pos]
      refreshLine(ls)
    of CTRL_A.char: # go to the start of the line
      editMoveHome(ls)
    of CTRL_E.char: # go to the end of the line
      editMoveEnd(ls)
    of CTRL_L.char: # clear screen
      clearScreen()
      refreshLine(ls)
    of CTRL_W.char: # delete previous word
      editDeletePrevWord(ls)
    else:
      try:
        editInsert(ls, c)
        ls[].truebuf = ls[].buf
      except:
        raise newException(ValueError, "") # TODO: better exceptions
        # return -1

  return ls.buf


proc printKeyCodes* =
  ## This special mode is used by linenoise in order to print scan codes
  ## on screen for debugging / development purposes. It is implemented
  ## by the linenoise_example program using the --keycodes option.
  var quit = "    "

  echo ("Linenoise key codes debugging mode.\n" &
    "Press keys to see scan codes. Type 'quit' at any time to exit.\n")
  enableRawMode(STDIN_FILENO)
  while true:
    let c = stdin.readChar
    quit = quit[1..^1] & c # shift string to left, then insert current char
                           # on the right.
    if quit == "quit": break

    echo "'$#' $# ($#) (type quit to exit)\n" % [
      if c in {'\x20'..'\x7E'}:
        $c 
      else:
        "?",
      c.int.toHex, $c.int
    ]
    echo "\r" # Go left edge manually, we are in raw mode.
    stdout.flushFile()

  disableRawMode(STDIN_FILENO)

proc raw(prompt: string): string =
  ## This function calls the line editing function edit() using
  ## the STDIN file descriptor set in raw mode.
  enableRawMode(STDIN_FILENO)
  result = edit(stdin, stdout, prompt)
  disableRawMode(STDIN_FILENO)
  echo ""

proc noTTY: string =
  ## This function is called when linenoise() is called with the standard
  ## input file descriptor not attached to a TTY. So for example when the
  ## program using linenoise is called in pipe or with a file redirected
  ## to its standard input. In this case, we want to be able to return the
  ## line regardless of its length (by default we are limited to 4k).
  return stdin.readLine()


proc linenoise*(prompt: string): string =
  ## The high level function that is the main API of the linenoise library.
  ## This function checks if the terminal has basic capabilities, just checking
  ## for a blacklist of stupid terminals, and later either calls the line
  ## editing function or uses dummy fgets() so that you will be able to type
  ## something even in the most desperate of the conditions.
  var buf: string
  if (not stdin.isAtty):
    # Not a tty: read from file / pipe. In this mode we don't want any
    # limit to the line size, so we call a function to handle that.
    return noTTY()
  elif (isUnsupportedTerm()):
    stdout.write(prompt)
    stdout.flushFile()
    buf = stdin.readLine()
    if buf.len > LINENOISE_MAX_LINE:
      buf = buf[0..<LINENOISE_MAX_LINE]
    var strlen = buf.len
    while strlen > 0 and (buf[strlen-1] == '\n' or buf[strlen-1] == '\r'):
      dec strlen
      buf = buf[0..<strlen]
    return buf
  else:
    try:
      buf = raw(prompt)
    except Exception as e:
      lnDebug("error", getStackTrace(e), getCurrentExceptionMsg())
    return buf

# Not needed by Nim.
# This is just a wrapper the user may want to call in order to make sure
# the linenoise returned buffer is freed with the same allocator it was
# created with. Useful when the main program is using an alternative
# allocator.
# void free(void *ptr) {}

# ================================= History ================================== #

# Not needed by Nim.
# Free the history, but does not reset it. Only used when we have to
# exit() to avoid memory leaks are reported by valgrind & co.
# static void freeHistory(void) {}

# At exit we'll try to fix the terminal to the initial conditions.
# static void atExit(void) {}

proc historyAdd*(line: string): int =
  ## This is the API call to add a new entry in the linenoise history.
  ## It uses a fixed array of char pointers that are shifted (memmoved)
  ## when the history max length is reached in order to remove the older
  ## entry and make room for the new one, so it is not exactly suitable for huge
  ## histories, but will work well for a few hundred of entries.
  ##
  ## Using a circular buffer is smarter, but a bit more complex to handle.
  if (history_max_len == 0):
    return 0

  # Initialization on first call.
  if (history.len == 0):
    history = newSeqOfCap[string](history_max_len.Natural)

  # Don't add duplicated lines.
  if (history.len > 0 and history[^1] == line):
    return 0

  # Add an heap allocated copy of the line in the history.
  # If we reached the max length, remove the older line.
  let linecopy = line
  if (linecopy == ""):
    return 0
  if (history.len == history_max_len):
    history = history[1..^1]

  history.add(linecopy)
  return 1

proc historySetMaxLen*(setlen: int): int =
  ## Set the maximum length for the history. This function can be called even
  ## if there is already some history, the function will make sure to retain
  ## just the latest 'len' elements if the new history length value is smaller
  ## than the amount of items already inside the history.

  if (setlen < 1):
    return 0
  if (history.len > 0):
    history = history[(history.len-setlen)..^1]

  history_max_len = setlen
  if (history.len > history_max_len):
    history = history[(history.len-history_max_len)..^1]
  return 1

proc historySave*(filename: string): int =
  ## Save the history in the specified file. On success 0 is returned
  ## otherwise -1 is returned.
  let old_umask: Mode = umask(S_IXUSR.Mode or S_IRWXG.Mode or S_IRWXO.Mode)
  let file = open(filename, fmReadWrite)
  discard umask(old_umask)
  filename.setFilePermissions({fpUserRead, fpUserWrite})
  for j in 0..<history.len:
    file.write("$#\n" % history[j])
  file.close()
  return 0

proc historyLoad*(filename: string): int =
  ## Load the history from the specified file. If the file does not exist
  ## zero is returned and no operation is performed.
  ##
  ## If the file exists and the operation succeeded 0 is returned, otherwise
  ## on error -1 is returned.

  let file = open(filename, fmRead)
  var buf = ""

  while true:
    buf = file.readLine()
    if buf.len > LINENOISE_MAX_LINE:
      buf = buf[0..<LINENOISE_MAX_LINE]

    discard historyAdd(buf)

  file.close()
  return 0
