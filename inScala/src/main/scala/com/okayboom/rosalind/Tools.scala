package com.okayboom.rosalind

import java.io.PrintStream
import scala.io.Source

object Tools {

  type Chars = Stream[Char]
  type Lines = Stream[String]

  def stdIn: Chars = Stream
    .continually(System.in.read)
    .takeWhile(_ != -1)
    .map(_.toChar)

  def fileIn(name: String): Chars = {
    val file = new java.io.File(name);

    io.Source.fromFile(
      file, io.Codec.ISO8859.name).toStream
  }

  def stdInLines: Lines = Source
    .stdin.getLines().toStream

  def stdOut(chars: Chars): Unit =
    stdOut(chars.mkString)

  def stdOut(str: String): Unit =
    System.out.print(str)
}
