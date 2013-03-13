package cernoch.sm.learn

/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
object Crossvalidation {
  
  def leaveOneOut[T]
    (exs: Iterable[T])
  = exs.map { target => (ex: T) => ex == target }

  def nFold[T]
    ( folds: Int,
      exs: Iterable[T])
  = {
    val index = (exs zip (Stream from 0)) toMap

    for (fold <- 1 to folds)
      yield (ex: T) =>
        (index(ex) % folds) == fold - 1
  }
}


