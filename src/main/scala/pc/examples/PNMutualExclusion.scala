package pc.examples

export pc.modelling.PetriNet
import pc.utils.MSet

object PNMutualExclusion:

  enum Place:
    case N,T,C
    
  export Place.*
  export pc.modelling.PetriNet.*
  export pc.modelling.SystemAnalysis.*

  // DSL-like specification of A Petri Net
  def mutualExclusionSystem() = toSystem(PetriNet[Place](
    MSet(N) ~~> MSet(T),
    MSet(T) ~~> MSet(C) ^^^ MSet(C),
    MSet(C) ~~> MSet())
  )

@main def mainPNMutualExclusion =
  import PNMutualExclusion.*
  // example usage
  println(mutualExclusionSystem().paths(MSet(N,N),7).toList.mkString("\n"))
