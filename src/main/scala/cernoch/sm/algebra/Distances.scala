package cernoch.sm.algebra

import cernoch.scalistics.collection.immutable.Hist

/**
 * Distance functions on distributions
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Distances {

  /**
   * Use distence metric ''f'' on discrete
   * distributions ''collection'' and ''b''.
   */
  private def applyDiscrete
    [T, N: Numeric, R:Numeric]
    (a: Hist[T,N], b: Hist[T,N], f: (N,N) => R)
  = {
    ( // This is collection performance improvement:
      if (a.cutPoints.keys == b.cutPoints.keys) {
        a.cutPoints.keys
      } else {
        (Set[T]() ++ a.cutPoints.keys ++ b.cutPoints.keys)
      } // Here starts the real procedure:
      ).foldLeft(f(a.above, b.above)){
      (x,v) => implicitly[Numeric[R]].plus(x, f(
        a.cutPoints.get(v).getOrElse(implicitly[Numeric[N]].zero),
        b.cutPoints.get(v).getOrElse(implicitly[Numeric[N]].zero))
      )
    }
  }

  /** Bhattacharya distance */
  def bhattacharya[T,N:Fractional]
    (a: Hist[T,N], b: Hist[T,N])
  = {
		val _i = implicitly[Fractional[N]]; import _i._

    val denom = times(a.sum, b.sum)
    applyDiscrete(
			a,
			b,
			(x:N,y:N) => scala.math.sqrt( div(times(x,y),denom).toDouble() )
		)
  }
}
