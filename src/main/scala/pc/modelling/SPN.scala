package pc.modelling

object SPN {

  // pre-conditions, rate, effects, inhibition
  type SPN[P] = Set[(MSet[P],MSet[P]=>Double,MSet[P],MSet[P])]

  def toPartialFunction[P](spn: SPN[P]): PartialFunction[MSet[P],Set[(Double,MSet[P])]] =
    {case m => for ((cond,rate,eff,inh)<-spn;
                    if (m disjoined inh);
                    r = rate(m);
                    out <- m extract cond) yield (r,out union eff) }

  def toCTMC[P](spn: SPN[P]): CTMC[MSet[P]] = CTMC.ofFunction(toPartialFunction(spn))

  def apply[P](transitions: (MSet[P],MSet[P]=>Double,MSet[P],MSet[P])*): SPN[P] = transitions.toSet

  // Syntactic sugar to write transitions as:  (a,b,c) ~~> (d,e)
  implicit final class LeftTransitionRelation[A](private val self: MSet[A]){
    def ~~> (y: MSet[A]): Tuple3[MSet[A], MSet[A], MSet[A]] = Tuple3(self, y, MSet[A]())
  }
  // Syntactic sugar to write transitions as:  MSet(a,b,c) ~~> MSet(d,e) ~~ MSet(f)
  implicit final class RightTransitionRelation[A](private val self: Tuple3[MSet[A],MSet[A],MSet[A]]){
    def ^^^ (z: MSet[A]): Tuple3[MSet[A], MSet[A],MSet[A]] = Tuple3(self._1, self._2, z)
  }
}