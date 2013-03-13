package cernoch.sm.algebra

import collection.mutable.ArrayBuffer

/**
 * Tools on generic collection
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Collections {

  /** All items in the collection are equal **/
  def allEqual[T]
    (i: Iterable[T])
  = {
    var same = true
    var last: T = i.head
    for (item <- i.tail) {
      same = same && (item == last)
      last = item
    }
    same
  }

  /** Carthesian product of sets */
  def cartProd[T]
    (data: Iterable[Iterable[T]])
    (merge: (T,T) => T)
  : Iterable[T]
  = data.size match {
    case 0 => throw new NoSuchElementException("Kix")
    case 1 => data.iterator.next
    case _ =>
      for(xh <- data.head;
          xt <- cartProd(data.tail)(merge))
      yield merge(xh,xt)
  }

  /** Differentiate each element the previous one */
  def differentiate[T]
    (collection: Iterable[T])
    (implicit num: Numeric[T])
  = {
    val out = ArrayBuffer[T]()
    var last = collection.head
    for (curr <- collection.tail) {
      out += num.minus(curr, last)
      last = curr
    }
    out
  }
}
