import junit.framework.TestCase
import scala.io.Source
import org.junit.Assert._
import com.okayboom.rosalind.Tools._
import java.nio.CharBuffer
import java.nio.ByteBuffer

class ExampleTest extends TestCase {

  def testDNA {
    val input: Chars = resource("dna/small.in")
    val expected: Chars = resource("dna/small.out")

    assertStreams(expected, DNA_parallel(input.par))
  }

  def testDNA_mapped {
    val input: ByteBuffer = resourceBuffer("dna/small.in")
    val expected: Chars = resource("dna/small.out")
    assertStreams(expected, DNA_mapped(input))
  }

  def testDNA_mapped_bare {
    val input: ByteBuffer = resourceBuffer("dna/small.in")
    val expected: Chars = resource("dna/small.out")
    assertStreams(expected, DNA_mapped_bare(input))
  }

  def testTargetSize {
    assertEquals(
      DNA_ultimate.minPartitionSize,
      DNA_ultimate.targetSize(0 until 10))
  }

  def testUltimateTargetSize {
    assertEquals(
      DNA_ultimate.minPartitionSize,
      DNA_ultimate.targetSize(0 until 10))
  }

  def testUltimateSplit {
    assertEquals(
      (0 until 5) :: (5 until 10) :: Nil,
      DNA_ultimate.split(0 until 10, 5))
    assertEquals(
      (1 until 6) :: (6 until 9) :: (9 until 12) :: Nil,
      DNA_ultimate.split(1 until 12, 5))
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

  def resourceBuffer(path: String): ByteBuffer = {
    val str = Source.fromResource(path).mkString
    ByteBuffer.wrap(str.getBytes())
  }

  def assertStreams(expected: Chars, actual: Chars) =
    assertEquals(expected.mkString, actual.mkString)

  implicit def stingToStream(s: String): Chars =
    s.toCharArray().toStream
}
