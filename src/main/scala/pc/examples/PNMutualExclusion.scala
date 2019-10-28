package pc.examples

import pc.modelling.PetriNet
import pc.modelling.PetriNet._
import pc.utils.MSet

object PNMutualExclusion extends App {

  object place extends Enumeration {
    val N,T,C = Value
  }
  type Place = place.Value
  import place._

  // DSL-like specification of A Petri Net
  def mutualExclusionSystem() = toSystem(PetriNet[Place](
    MSet(N) ~~> MSet(T),
    MSet(T) ~~> MSet(C) ^^^ MSet(C),
    MSet(C) ~~> MSet())
  )

  // example usage
  println(mutualExclusionSystem().paths(MSet(N,N),7).toList.mkString("\n"))
}
