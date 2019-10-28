package pc.modelling

import pc.utils.MSet

object PetriNet {
  // pre-conditions, effects, inhibition
  type PetriNet[P] = Set[(MSet[P],MSet[P],MSet[P])]

  // factory of A Petri Net
  def apply[P](transitions: (MSet[P],MSet[P],MSet[P])*): PetriNet[P] =
    transitions.toSet

  def toPartialFunction[P](pn: PetriNet[P]): PartialFunction[MSet[P],Set[MSet[P]]] =
    {case m => for ((cond,eff,inh)<-pn;
                    if (m disjoined inh);
                    out <- m extract cond) yield out union eff }

  // factory of A System
  def toSystem[P](pn: PetriNet[P]): System[MSet[P]] =
    System.ofFunction( toPartialFunction(pn))

  // Syntactic sugar to write transitions as:  (s,b,C) ~~> (d,e)
  implicit final class LeftTransitionRelation[P](private val self: MSet[P]){
    def ~~> (y: MSet[P]): Tuple3[MSet[P], MSet[P], MSet[P]] = Tuple3(self, y, MSet[P]())
  }
  // Syntactic sugar to write transitions as:  MSet(s,b,C) ~~> MSet(d,e) ~~ MSet(f)
  implicit final class RightTransitionRelation[P](
    private val self: Tuple3[MSet[P],MSet[P],MSet[P]]
  ){
    def ^^^ (z: MSet[P]): Tuple3[MSet[P], MSet[P],MSet[P]] = Tuple3(self._1, self._2, z)
  }
}