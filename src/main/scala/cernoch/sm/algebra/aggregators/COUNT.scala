package cernoch.sm.algebra.aggregators

class COUNT[N]
  (private var i: N)
  (implicit num: Numeric[N])
  extends Aggregator[Any,N] {
  import num._

   def apply = Some(i)

   def name = "count"

   def +(v: Any) = new COUNT(plus(i, one))
  def +=(v: Any) { i = plus(i, one) }

   def ++(v: Iterable[Any]) = v.foldLeft[Aggregator[Any,N]](this){_ + _}
   def ++=(v: Iterable[Any]) { for (_ <- v) { i = plus(i, one) } }
 }

object COUNT {

  def usingInt = new COUNT(0)
  def usingLong = new COUNT(0l)
  def usingFloat = new COUNT(0f)
  def usingDouble = new COUNT(0d)

  def usingBigInt = new COUNT(BigInt(0))
  def usingBigDecimal = new COUNT(BigDecimal(0))

  def apply[T]
  (seq: Any*)(implicit num: Numeric[T])
  = (new COUNT[T](num.zero) ++ seq).apply.get
}
