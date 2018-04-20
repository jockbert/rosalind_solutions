import com.okayboom.rosalind.DnaSum
import com.okayboom.rosalind.Tools._

object DNA extends App {

  def apply(input: Stream[Char]): String = {

    val sum: DnaSum = input
      .aggregate(DnaSum.ZERO)(
        (sum, c) => sum.inc(c),
        (sum1, sum2) => sum1 + sum2)

    s"${sum.a} ${sum.c} ${sum.g} ${sum.t}\n"
  }

  stdOut(apply(stdIn))
}
