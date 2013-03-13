package cernoch.sm.space

/**
 * Thrown when evaluating collection state,
 * which should not be explored at all
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class ThrowAway(msg: String)
  extends RuntimeException(msg) {}

/**
 * Thrown when evaluating collection state,
 * which is too immature to be evaluated
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class Reconsider(msg: String)
  extends RuntimeException(msg) {}
