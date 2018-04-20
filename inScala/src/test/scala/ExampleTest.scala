import junit.framework.TestCase
import scala.io.Source
import org.junit.Assert._

class ExampleTest extends TestCase {

  type Chars = Stream[Char]

  def testDNA {
    assertStreams("4 3 2 1\n", DNA("AAAA CCC GG T"))

    val input: Chars = resource("dna/small.in")
    val expected: Chars = resource("dna/small.out")

    assertStreams(expected, DNA(input))

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

  def resource(path: String): Chars =
    Source.fromResource(path).toStream

  def assertStreams(expected: Chars, actual: Chars) =
    assertEquals(expected.mkString, actual.mkString)

  implicit def stingToStream(s: String): Chars =
    s.toCharArray().toStream
}
