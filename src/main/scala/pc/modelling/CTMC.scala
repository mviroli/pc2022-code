package pc.modelling

import scala.util.Random

trait CoreCTMC[S] {
  // rate + state
  def transitions(a: S): Set[(Double,S)]
}

trait CTMC[S] extends CoreCTMC[S] with System[S] {

  def next(s: S): Set[S] = transitions(s) map (_._2)

  def transitions(s: S): Set[(Double,S)]

}

object CTMC {

  def ofFunction[S](f: PartialFunction[S,Set[(Double,S)]]): CTMC[S] =
    new CoreCTMC[S] with CTMC[S]{
      override def transitions(s: S) = f.applyOrElse(s, (x: S)=>Set[(Double,S)]())
    }

  def ofRelation[S](rel: Set[(S,Double,S)]): CTMC[S] = ofFunction{
    case s => rel filter {_._1 == s} map { t=>(t._2,t._3)}
  }

  def ofTransitions[S](rel: (S,Double,S)*): CTMC[S] = ofRelation(rel.toSet)
}