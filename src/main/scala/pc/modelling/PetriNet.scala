package pc.modelling

object PetriNet {

  // pre-conditions, effects, inhibition
  type PetriNet[P] = Set[(MSet[P],MSet[P],MSet[P])]

  def toPartialFunction[P](pn: PetriNet[P]): PartialFunction[MSet[P],Set[MSet[P]]] =
    {case m => for ((cond,eff,inh)<-pn;
                    if (m disjoined inh);
                    out <- m extract cond) yield out union eff }

  def toSystem[P](pn: PetriNet[P]): System[MSet[P]] = System.ofFunction( toPartialFunction(pn))

  def apply[P](transitions: (MSet[P],MSet[P],MSet[P])*): PetriNet[P] = transitions.toSet

  // Syntactic sugar to write transitions as:  (a,b,c) ~~> (d,e)
  implicit final class LeftTransitionRelation[A](private val self: MSet[A]){
    def ~~> (y: MSet[A]): Tuple3[MSet[A], MSet[A], MSet[A]] = Tuple3(self, y, MSet[A]())
  }
  // Syntactic sugar to write transitions as:  MSet(a,b,c) ~~> MSet(d,e) ~~ MSet(f)
  implicit final class RightTransitionRelation[A](private val self: Tuple3[MSet[A],MSet[A],MSet[A]]){
    def ^^^ (z: MSet[A]): Tuple3[MSet[A], MSet[A],MSet[A]] = Tuple3(self._1, self._2, z)
  }
}