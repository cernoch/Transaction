package cernoch.sm.algebra

package object aggregators {
  private implicit def long2float(l: Long) = l.toFloat
  private implicit def long2double(l: Long) = l.toDouble
  private implicit def long2bigDec(l: Long) = BigDecimal(l)
  private implicit def bigInt2bigDec(l: BigInt) = BigDecimal(l)
}
