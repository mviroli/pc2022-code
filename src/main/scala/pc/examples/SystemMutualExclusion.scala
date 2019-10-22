package pc.examples

import pc.modelling.System

object SystemMutualExclusionion extends App {

  object state extends Enumeration {
    val N,T,C = Value
  }
  type State = state.Value
  import state._

  // System specification
  def mutualExclusion(size: Int): System[List[State]] = {
    def moveOne(l:List[State])(from: State, to: State): Set[List[State]] =
      for (i <- (0 until size).toSet; if l(i)==from) yield l.updated(i,to)
    System.ofFunction[List[State]]{case l =>
      moveOne(l)(N,T) ++ moveOne(l)(C,N) ++
        (if (l.contains(C)) Set() else moveOne(l)(T,C))
    }
  }

  println(mutualExclusion(3).next(List(N,N,N)))
  println(mutualExclusion(3).next(List(N,T,T)))
  println(mutualExclusion(3).next(List(N,T,C)))

  println(mutualExclusion(3).paths(List(N,N,N),5).toList)
  println(mutualExclusion(3).paths(List(N,N,N),5).contains(
    List(List(N, N, N), List(T, N, N), List(T, T, N), List(C, T, N), List(N, T, N))))
}
