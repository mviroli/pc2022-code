package pc.examples

import pc.modelling.{CTMCAnalysis, PetriNet, SPN}
import pc.utils.MSet
import java.util.Random

object StochasticMutualExclusion extends App {
  // Specification of my data-type for states
  object place extends Enumeration {
    val N,T,C = Value
  }
  type Place = place.Value
  import SPN._
  import place._

  val spn = SPN[Place](
    (MSet(N), m=>1.0,MSet(T),MSet()),
    (MSet(T), m=>m(T),MSet(C),MSet(C)),
    (MSet(C), m=>2.0,MSet(),MSet()))


  val rwAnalysis = CTMCAnalysis(toCTMC(spn))
  println(rwAnalysis.newSimulationTrace(MSet(N,N,N,N),new Random)
                    .take(20)
                    .toList.mkString("\n"))
}