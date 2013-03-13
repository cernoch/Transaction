package cernoch.sm.algebra

import cernoch.scalistics.interval._
import scala.collection.{mutable => mut}
import scala.collection.immutable.TreeSet

/**
 * Intervals related operations
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Intervals {

  /** Is the collection collection sequence of "touching" intervals? */
  def touching[T]
  (i: Iterable[Interval[T, Open[T], Closed[T]]])
  = {
    var continuous = true
    var last: Endpoint[T] = null

    for (ivl <- i) {

      if (last != null)
        continuous = continuous && last == ivl.l

      last = ivl.r
    }
    continuous
  }

  /** Intersections of all intervals in all dimensions */
  def intersections
    [T, L <: Endpoint[T],R <: Endpoint[T]]
    (d: Iterable[Iterable[Interval[T,L,R]]])
    (implicit ordering: Ordering[Endpoint[T]])
  = {
    // All dimensions are equal =>
    if (Collections.allEqual(d)) {
      d.head

    } else {
      // To be returned
      val out = mut.HashSet[Interval[T,L,R]]()

      // Index by L and R item
      val idx = d.map{ dim =>
        ( dim groupBy {_.l.asInstanceOf[Endpoint[T]]},
          dim groupBy {_.r.asInstanceOf[Endpoint[T]]},
          new mut.HashSet[Interval[T,L,R]]()
          )
      }

      // Helper list
      val bufs = idx.map{_._3}

      // All endpoints
      val all = idx.foldLeft(TreeSet.empty){(x,y) => {
        x ++ y._1.keys ++ y._2.keys
      }}

      for (cut <- all) {

        for ((lIdx,_, buf) <- idx) {
          buf ++= lIdx.get(cut).getOrElse(Nil)
        }

        for (isect <- Collections.cartProd(bufs){_ intersect _})
          if (ordering.lt(isect.l, isect.r))
            out += isect


        for ((_,rIdx,buf) <- idx) {
          buf --= rIdx.get(cut).getOrElse(Nil)
        }
      }

      out
    }
  }
}
