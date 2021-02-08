import unittest
import linenoise

suite "basic functionality":

  test "single line mode":
    while true:
      let line = linenoise("test>")
      echo line
      if line == "end":
        break

  test "completions":
    proc completionCallback(buf: string): seq[string] =
      if buf.len == 0: return

      if buf == "t":
        return @["test"]
    setCompletionCallback(completionCallback)

    while true:
      let line = linenoise(">>>")
      echo line
