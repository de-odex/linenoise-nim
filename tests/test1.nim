import unittest
import linenoise

suite "basic functionality":

  test "single line mode":
    while true:
      let line = readLine("test>")
      echo line

  test "completions":
    proc completionCallback(buf: string): seq[string] =
      if buf.len == 0: return

      if buf == "t":
        return @["test"]
    setCompletionCallback(completionCallback)

    while true:
      let line = readLine(">>>")
      echo line
