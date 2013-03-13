package cernoch.sm.algebra

import cernoch.scalistics.collection.immutable.Hist
import scala.collection.mutable.ArrayBuffer
import Collections._

/**
 * Routines on general histograms
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Histograms {

  def lakes[T:Numeric,N:Numeric]
	(hist: Hist[T,N])
  = {
		val _i = implicitly[Numeric[N]]; import _i._

    var o = List[(T,List[(T,N)])]()
    val buffer = ArrayBuffer[(T,N)]()

    var rise = true
    var lmax: N = zero
    var last: N = hist.above
		var lastRise: T = 0.asInstanceOf[T]

    def push {
      val peak = min(lmax,last)
      val NONE = List[(T,N)]()
      val aligned = buffer.foldLeft(NONE)(
        (list,x) => {
          val skip = peak - x._2
          if (lteq(skip,zero))
            list
          else
            ((x._1, skip)) :: list
        })

      o = (lastRise, aligned) :: o
    }

    for ((item,curr) <- hist.cutPoints.toArray.reverse) {
      val diff = curr - last

      // A peak WAS reached
      if (rise && lt(diff,zero)) {
        if (lmax != zero) push
        buffer.clear
        lmax = last
      }

      if (rise) {
        if (lt(diff,zero))
          rise = false
      } else {
        if (lt(zero,diff))
          rise = true
      }
      // Items since the last peak
      buffer += ((item,curr))
      last = curr
      if (lt(zero,diff))
        lastRise = item
    }
    if (rise && lmax != zero)
      push
    o
  }

  private def alp
	[T: Fractional, N: Numeric]
  (hist: Hist[T,N])
  (area: (T,T,N) => T)
  = {(
    for ((beg,lake) <- lakes(hist))
    yield {
      var last = beg
      var suma = implicitly[Fractional[T]].zero
      for ((value, depth) <- lake) {
        suma = implicitly[Fractional[T]]
					.plus(suma, area(value, last, depth))
        last = value
      }
      suma
    }).sorted }

  private def noise
	[T: Fractional]
  (i: Iterable[T])
  = {
		val _i = implicitly[Fractional[T]]; import _i._

    var y = zero
    for (x <- i) yield {
      y = plus(y,one)
      div(x,y)
    }
  }

  private def maxOrZero
	[T: Numeric]
  (i: Iterable[T])
  = if (i.isEmpty)
    implicitly[Numeric[T]].zero
  else
    i.max

  def ncALP[T:Fractional, N: Numeric]
	(hist: Hist[T,N],
	 conv: N => T)
  = {
		val _i = implicitly[Fractional[T]]; import _i._
		maxOrZero(noise(differentiate(
    	zero :: alp(hist){ (x,y,h) => times(conv(h),minus(x,y)) }
  	)))
	}
}
