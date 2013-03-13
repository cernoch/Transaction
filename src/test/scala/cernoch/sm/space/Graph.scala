package cernoch.sm.space

class Graph[T](val g: Map[T,Set[T]]) {
  def +=(node: (T, Set[T])) = new Graph(g + node)
}

object Graph {
  def apply[T]() = new Graph[T](Map())
}
