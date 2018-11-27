package com.okayboom.rosalind

import java.io.PrintStream
import scala.io.Source
import java.nio.file.Files
import java.nio.channels.FileChannel
import java.io.RandomAccessFile
import java.nio.MappedByteBuffer
import java.nio.CharBuffer
import java.io.File
import java.nio.ByteBuffer

object Tools {

  type Chars = Stream[Char]
  type Lines = Stream[String]

  def stdIn: Chars = Stream
    .continually(System.in.read)
    .takeWhile(_ != -1)
    .map(_.toChar)

  def fileIn(name: String): Source = {
    val file = new java.io.File(name);

    io.Source.fromFile(
      file, io.Codec.ISO8859.name)
  }

  def memoryMappedFile(name: String): ByteBuffer = {
    val file: RandomAccessFile = new RandomAccessFile(name, "r");
    val fileChannel: FileChannel = file.getChannel();
    val buffer: MappedByteBuffer = fileChannel.map(FileChannel.MapMode.READ_ONLY, 0, file.length());
    buffer
  }

  def stdInLines: Lines = Source
    .stdin.getLines().toStream

  def stdOut(chars: Chars): Unit =
    stdOut(chars.mkString)

  def stdOut(str: String): Unit =
    System.out.print(str)
}
