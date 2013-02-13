package cernoch.sm.algebra.aggregators

import collection.immutable.TreeMap

abstract class TreeCounter[T,N]
  (var map: TreeMap[T,N])
  (implicit num: Numeric[N])
extends Symmetric[T] {
  import TreeCounter._

  protected def create(map: TreeMap[T,N]): Symmetric[T]

  def +(v: T) = create(inc(map,v))
  def +=(v: T) { map = inc(map,v) }
  def ++(v: Iterable[T]) = create(v.foldLeft(map)(inc[T,N]))
  def ++=(v: Iterable[T]) { map = v.foldLeft(map)(inc[T,N]) }
}

private[aggregators] object TreeCounter {

  def inc[T,N]
    (m: TreeMap[T, N], v: T)
    (implicit num: Numeric[N])
  = {
    import num._
    m + (v -> m.get(v).map(plus(_,one)).getOrElse(one) )
  }
}
