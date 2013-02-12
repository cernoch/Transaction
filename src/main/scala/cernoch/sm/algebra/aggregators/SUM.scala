package cernoch.sm.algebra.aggregators

class SUM[T]
  (private var suma: T)
  (implicit num: Numeric[T])
extends Symmetric[T] {

   import num._

   def name = "sum"
   def apply = Some(suma)

   def +(v: T) = new SUM[T](plus(suma, v))
   def +=(v: T) {  suma = plus(suma, v) }

   def ++(v: Iterable[T]) =  new SUM[T](plus(suma, v.foldLeft(zero){plus}))
   def ++=(v: Iterable[T]) {  suma = plus (suma, v.foldLeft(zero){plus}) }
 }

object SUM {

  def forInt = new SUM(0)
  def forLong = new SUM(0l)
  def forFloat = new SUM(0f)
  def forDouble = new SUM(0d)

  def forBigInt = new SUM(BigInt(0))
  def forBigDecimal = new SUM(BigDecimal(0))

  def apply[T]
  (seq: T*)(implicit num: Numeric[T])
  = (new SUM[T](num.zero) ++ seq).apply.get
}

