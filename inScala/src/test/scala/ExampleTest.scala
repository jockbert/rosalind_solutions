import junit.framework.TestCase
import scala.io.Source
import org.junit.Assert._
import com.okayboom.rosalind.Tools._

class ExampleTest extends TestCase {

  def testDNA {
    val input: Chars = resource("dna/small.in")
    val expected: Chars = resource("dna/small.out")

    assertStreams(expected, DNA(input.par))
  }

  def testRNA {
    assertStreams("AAAA CCC GG U", RNA("AAAA CCC GG T"))

    val input: Chars = resource("rna/small.in")
    val expected: Chars = resource("rna/small.out")

    assertStreams(expected, RNA(input))
  }

  def testREVC {
    assertStreams("A CC GGG TTTT", REVC("AAAA CCC GG T"))

    val input: Chars = resource("revc/small.in")
    val expected: Chars = resource("revc/small.out")

    assertStreams(expected, REVC(input))
  }

  def testSUBS {

    assertStreams(
      "2 5 6 15 17 18",
      SUBS("AUGCUUCAGAAAGGUCUUACG" :: "U" :: Nil toStream))

    val input: Lines = resourceLines("subs/small.in")
    val expected: Chars = resource("subs/small.out")

    assertStreams(expected, SUBS(input))
  }

  def resourceLines(path: String): Lines =
    Source.fromResource(path).getLines().toStream

  def resource(path: String): Chars =
    Source.fromResource(path).toStream

  def assertStreams(expected: Chars, actual: Chars) =
    assertEquals(expected.mkString, actual.mkString)

  implicit def stingToStream(s: String): Chars =
    s.toCharArray().toStream
}
