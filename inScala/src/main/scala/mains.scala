import com.okayboom.rosalind.DnaSum
import com.okayboom.rosalind.Tools._
import scala.annotation.tailrec
import scala.collection.parallel.ParSeq
import scala.util.Random
import scala.collection.GenSeq
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.PrintWriter
import scala.collection.parallel.ParIterable
import java.nio.CharBuffer
import java.nio.ByteBuffer

object GenerateData extends App {

  def printUsage(message: String): Unit = {
    println(message)
    println("Usage: <file_size> <character_pool>")
    System.exit(1)
  }

  def toInt(s: String): Option[Int] =
    try Some(s.toInt)
    catch { case e: Exception => None }

  def generator(pool: String)(): Char =
    pool.charAt(Random.nextInt(pool.length()))

  if (args.length != 2) printUsage(
    "Bad number of arguments " + args.length + " != 2.")

  val maybeSize = toInt(args(0))

  if (maybeSize.isEmpty) printUsage(
    "Bad file size '" + args(0) + "'")

  val size = maybeSize.get

  if (size < 0) printUsage(
    "Bad file size " + size)

  val charSource = generator(args(1)) _
  val out = new PrintWriter(new BufferedOutputStream(System.out))

  for (_ <- 0 until size) out.append(charSource())
  out.println()
  out.flush()
}

/** Count DNA Nucleotides. */
object DNA_parallel extends App {

  def apply(input: ParIterable[Char]): String = {

    val sum: DnaSum = input
      .aggregate(DnaSum.ZERO)(
        (sum, c) => sum.inc(c),
        (sum1, sum2) => sum1 + sum2)

    s"${sum.a} ${sum.c} ${sum.g} ${sum.t}\n"
  }

  stdOut(apply(fileIn(args(0)).getLines().mkString("").par))
}

/** Count DNA Nucleotides in single thread. */
object DNA_single extends App {

  def apply(input: Iterator[Char]): String = {

    val sum: DnaSum = input
      .foldLeft(DnaSum.ZERO)(
        (sum, c) => sum.inc(c))

    s"${sum.a} ${sum.c} ${sum.g} ${sum.t}\n"
  }

  stdOut(apply(fileIn(args(0)).toIterator))
}

/** Count DNA Nucleotides in single thread using memory mapped file. */
object DNA_mapped extends App {

  def apply(input: ByteBuffer): String = {
    var sum = DnaSum.ZERO

    while (input.hasRemaining()) {
      val char = input.get().asInstanceOf[Char]
      sum = sum.inc(char)
    }

    s"${sum.a} ${sum.c} ${sum.g} ${sum.t}\n"
  }

  stdOut(apply(memoryMappedFile(args(0))))
}

case class Sum(var a: Int, var c: Int, var g: Int, var t: Int) {}

/**
 * Count DNA Nucleotides in single thread using memory mapped file
 * and avoiding leaving "bread crumbs" for GC to pick up.
 */
object DNA_mapped_bare extends App {

  def apply(input: ByteBuffer): String = {
    val sum = Sum(0, 0, 0, 0)

    while (input.hasRemaining()) {
      val char = input.get().asInstanceOf[Char]
      char match {
        case 'A' => sum.a = sum.a + 1
        case 'C' => sum.c = sum.c + 1
        case 'G' => sum.g = sum.g + 1
        case 'T' => sum.t = sum.t + 1
        case _   =>
      }
    }

    s"${sum.a} ${sum.c} ${sum.g} ${sum.t}\n"
  }

  stdOut(apply(memoryMappedFile(args(0))))
}

/** Count DNA Nucleotides in tail recursive single thread. */
object DNA_recursive extends App {

  @tailrec
  def apply(input: Chars, sum: DnaSum = DnaSum.ZERO): String =
    if (input.isEmpty) s"${sum.a} ${sum.c} ${sum.g} ${sum.t}\n"
    else apply(input.tail, sum.inc(input.head))

  stdOut(apply(fileIn(args(0)).toStream))
}

/** Compare DNA and DNA_single. */
object DNA_compare extends App {

  def randomDNA(): Char =
    Vector('A', 'C', 'G', 'T')
      .apply(Random.nextInt(4))

  def generateDNA(size: Int, result: List[Char] = Nil): Seq[Char] =
    if (size <= 0) result
    else generateDNA(size - 1, randomDNA() :: result)

  def provokeGC() =
    for (_ <- 0 to 10) System.gc(); Thread.sleep(10)

  def mcroTime[A](task: () => A): (A, Long) = {
    // provoke garbage collection before mesure
    provokeGC()

    val startTime = System.nanoTime
    val result = task()
    val microDuration = (System.nanoTime - startTime) / 1000
    (result, microDuration)
  }

  val sizes = 10 :: 20 :: 50 ::
    100 :: 200 :: 500 ::
    1000 :: 2000 :: 5000 ::
    10000 :: 20000 :: 50000 ::
    100000 :: 200000 :: 500000 ::
    1000000 :: 2000000 :: 5000000 ::
    10000000 :: 20000000 :: 50000000 ::
    Nil

  println()
  println("Available processors: " + Runtime.getRuntime().availableProcessors())
  println()
  println("      Size     Single   Parallel    Speedup        Same")
  println("                 [µs]       [µs]     factor      result")
  println("-------------------------------------------------------")

  sizes.foreach(size => {
    val data: Seq[Char] = generateDNA(size)
    val parData: ParSeq[Char] = data.par
    val (singleRes, singleTime) = mcroTime(() => DNA_single(data.iterator))
    val (parallelRes, parallelTime) = mcroTime(() => DNA_parallel(parData))
    val aggrSpeedup = 1.0 * singleTime / parallelTime
    val sameRes = singleRes == parallelRes
    println(f"$size%,10d $singleTime%,10d $parallelTime%,10d $aggrSpeedup%,9.2f $sameRes%12b")
  })

}

/** Transcribing DNA into RNA. */
object RNA extends App {

  def apply(input: Chars): Chars = input
    .map(c => if (c == 'T') 'U' else c)

  stdOut(apply(stdIn))
}

/** Complementing a Strand of DNA. */
object REVC extends App {

  def apply(input: Chars): Chars = input
    .map(c => c match {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
      case x   => x
    })
    .reverse

  stdOut(apply(stdIn))
}

/** Finding a Motif in DNA. */
object SUBS extends App {

  def allMotifIndexes(data: String, motif: String): List[Int] = {

    @tailrec
    def loop(startIndex: Int = 0, res: List[Int] = Nil): List[Int] =
      data.indexOf(motif, startIndex) match {
        case -1 => res
        case n  => loop(n + 1, n :: res)
      }

    loop().reverse
  }

  def apply(input: Lines): String = {
    val data = input.head
    val motif = input.tail.head

    allMotifIndexes(data, motif)
      .map(_ + 1)
      .mkString(" ")
  }

  stdOut(apply(stdInLines))
}
