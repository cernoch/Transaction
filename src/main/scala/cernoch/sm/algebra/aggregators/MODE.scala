package cernoch.sm.algebra.aggregators

class MODE[T,N]
  (private[aggregators] var mapa: Map[T,N])
  (implicit num: Numeric[N])
extends Symmetric[T] {
  import MODE._

  override def name = "mode"

  def apply()
  = MAX(mapa.values.toList: _*) match {
    case None => None
    case Some(max)
    => mapa
      .filter{case (_,v) => v == max }
      .keys.toList match {
      case List(v) => Some(v)
      case v => None
    }
  }

  def +(v: T) =  new MODE[T,N](inc(mapa)(v))
  def +=(v: T) { mapa = inc(mapa)(v) }

  def ++(v: Iterable[T]) =  v.foldLeft(this){_ + _}
  def ++=(v: Iterable[T]) { mapa = v.foldLeft(mapa){inc(_)(_)} }
}


object MODE {

  private def inc[T,N]
    (m: Map[T, N])(v: T)
    (implicit num: Numeric[N])
  = {
    import num._
    m + (v -> m.get(v).map(plus(_,one)).getOrElse(one) )
  }


  def forFloat = new MODE(Map[Float,Long]())
  def forDouble = new MODE(Map[Double,Long]())
  def forBigDecimal = new MODE(Map[BigDecimal,BigInt]())

  def forString = new MODE(Map[String,Long]())

  def forInt = new MODE( Map[Int,Long]() )
  def forLong = new MODE(Map[Long,Long]() )
  def forBigInt = new MODE(Map[BigInt,BigInt]() )

  def apply[T](seq: T*) : Option[T] = {
    val agg = new MODE(Map[T,Long]())
    agg ++= seq
    agg()
  }
}



