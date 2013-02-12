package cernoch.sm.algebra.aggregators

abstract class MinMax[T]
  (private var value: T)
  extends Symmetric[T] {

    protected def create(v: T) : Symmetric[T]
    protected def select(a: T,  b: T) : T
    protected def compare(a: T,  b: T) : Boolean

    def apply = Option(value)

    def +(v: T) = if (value == null || compare(v,value)) create(v) else this
    def ++(v: Iterable[T]) = v.foldLeft[Aggregator[T,T]](this)(_ + _)

    def +=(v: T) { if (value == null || compare(v,value)) value = v }
    def ++=(v: Iterable[T]) { v foreach += }
   }


class MIN[T]
  (value: T)
  (implicit ord: Ordering[T])
  extends MinMax[T](value) {
  import ord._

  def name = "min"

  protected def create(v: T) = new MIN[T](v)
  protected def select(a: T, b: T) = min(a,b)
  protected def compare(a: T, b: T) = lt(a,b)
}

object MIN {
  def forInt = new MIN(null.asInstanceOf[Int])
  def forLong = new MIN(null.asInstanceOf[Long])
  def forFloat = new MIN(null.asInstanceOf[Float])
  def forDouble = new MIN(null.asInstanceOf[Double])

  def forBigInt = new MIN(null.asInstanceOf[BigInt])
  def forBigDecimal = new MIN(null.asInstanceOf[BigDecimal])

  def apply[T]
  (seq: T*)(implicit num: Ordering[T])
  = (new MIN[T](null.asInstanceOf[T]) ++ seq).apply
}




class MAX[T]
  (value: T)
  (implicit ord: Ordering[T])
  extends MinMax[T](value) {
  import ord._

  def name = "max"

  protected def create(v: T) = new MAX[T](v)
  protected def select(a: T, b: T) = max(a,b)
  protected def compare(a: T, b: T) = gt(a,b)
}

object MAX {
  def forInt = new MAX(null.asInstanceOf[Int])
  def forLong = new MAX(null.asInstanceOf[Long])
  def forFloat = new MAX(null.asInstanceOf[Float])
  def forDouble = new MAX(null.asInstanceOf[Double])

  def forBigInt = new MAX(null.asInstanceOf[BigInt])
  def forBigDecimal = new MAX(null.asInstanceOf[BigDecimal])

  def apply[T](seq: T*)
  (implicit num: Ordering[T])
  = (new MAX[T](null.asInstanceOf[T]) ++ seq).apply
}
