package pc.examples

import pc.modelling.{CTMCAnalysis, PetriNet, SPN}
import pc.utils.MSet
import java.util.Random

object TrySPN extends App {
  // Specification of my data-type for states
  object place extends Enumeration {
    val n,t,c = Value
  }
  type Place = place.Value
  import SPN._
  import place._

  val spn = SPN[Place](
    (MSet(n),m=>1.0,MSet(t),MSet()),
    (MSet(t),m=>m(t),MSet(c),MSet(c)),
    (MSet(c),m=>2.0,MSet(),MSet()))


  val rwAnalysis = CTMCAnalysis(toCTMC(spn))
  println(rwAnalysis.newSimulationTrace(MSet(n,n,n,n),new Random).take(20).toList.mkString("\n"))
}