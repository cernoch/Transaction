package cernoch.sm.algebra.aggregators

/**
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
trait Aggregator[T,O] extends (() => Option[O]) {

  def name : String

  def +(v: T): Aggregator[T,O]
  def ++(v: Iterable[T]): Aggregator[T,O]

  def +=(v: T)
  def ++=(v: Iterable[T])

  override def toString() = name + "=" + apply

  override def equals(o:Any)
  = o.isInstanceOf[Aggregator[_,_]] &&
    o.asInstanceOf[Aggregator[_,_]].name == name &&
    o.asInstanceOf[Aggregator[_,_]]() == this()

  override def hashCode() = toString().hashCode + 3 * apply().hashCode()
}



trait Symmetric[T] extends Aggregator[T,T]