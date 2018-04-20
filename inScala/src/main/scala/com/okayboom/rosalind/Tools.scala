package com.okayboom.rosalind

import java.io.PrintStream

object Tools {

  type Chars = Stream[Char]

  def stdIn: Chars = Stream
    .continually(System.in.read)
    .takeWhile(_ != -1)
    .map(_.toChar)

  def stdOut(chars: Chars): Unit =
    stdOut(chars.mkString)

  def stdOut(str: String): Unit =
    System.out.print(str)
}
