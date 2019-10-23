package pc.modelling

import pc.utils.MSet

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

  // Syntactic sugar to write transitions as:  (s,b,C) ~~> (d,e)
  implicit final class LeftTransitionRelation[P](private val self: MSet[P]){
    def ~~> (y: MSet[P]): Tuple3[MSet[P], MSet[P], MSet[P]] = Tuple3(self, y, MSet[P]())
  }
  // Syntactic sugar to write transitions as:  MSet(s,b,C) ~~> MSet(d,e) ~~ MSet(f)
  implicit final class RightTransitionRelation[P](private val self: Tuple3[MSet[P],MSet[P],MSet[P]]){
    def ^^^ (z: MSet[P]): Tuple3[MSet[P], MSet[P],MSet[P]] = Tuple3(self._1, self._2, z)
  }
}