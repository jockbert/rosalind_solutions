import com.okayboom.rosalind.DnaSum
import com.okayboom.rosalind.Tools._
import scala.annotation.tailrec

/** Count DNA Nucleotides. */
object DNA extends App {

  def apply(input: Chars): String = {

    val sum: DnaSum = input
      .aggregate(DnaSum.ZERO)(
        (sum, c) => sum.inc(c),
        (sum1, sum2) => sum1 + sum2)

    s"${sum.a} ${sum.c} ${sum.g} ${sum.t}\n"
  }

  stdOut(apply(stdIn))
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
