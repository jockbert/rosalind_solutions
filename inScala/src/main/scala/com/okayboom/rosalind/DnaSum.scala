package com.okayboom.rosalind

object DnaSum {
  val ZERO = DnaSum(0, 0, 0, 0)
}

case class DnaSum(val a: Int, val c: Int, val g: Int, val t: Int) {

  def incA() = this.copy(a = a + 1)
  def incG() = this.copy(g = g + 1)
  def incC() = this.copy(c = c + 1)
  def incT() = this.copy(t = t + 1)

  def +(other: DnaSum) = DnaSum(
    a = this.a + other.a,
    c = this.c + other.c,
    g = this.g + other.g,
    t = this.t + other.t)

  def inc(c: Char): DnaSum = c match {
    case 'A' => incA()
    case 'C' => incC()
    case 'G' => incG()
    case 'T' => incT()
    case _   => this
  }
}
