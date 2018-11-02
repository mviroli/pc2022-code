package examples

import pc.modelling.{MSet, PetriNet}

object TryPN extends App {
  // Specification of my data-type for states
  object place extends Enumeration {
    val n,t,c = Value
  }
  type Place = place.Value
  import place._
  import MSet._
  import PetriNet._

  val pn = PetriNet[Place](
    MSet(n) ~~> MSet(t),
    MSet(t) ~~> MSet(c) ^^^ MSet(c),
    MSet(c) ~~> MSet())

  val system = toSystem(pn)

  println(system.paths(MSet(n,n),7).toList.mkString("\n"))
}