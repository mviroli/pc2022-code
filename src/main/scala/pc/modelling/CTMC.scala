package pc.modelling

import scala.util.Random

trait CoreCTMC[A] {
  def nextWithRate(a: A): Set[(Double,A)]
}

trait CTMC[A] extends CoreCTMC[A] with System[A] {

  def next(a: A): Set[A] = nextWithRate(a) map (_._2)

  def nextWithRate(a: A): Set[(Double,A)]

}

object CTMC {

  def ofFunction[A](f: PartialFunction[A,Set[(Double,A)]]): CTMC[A] = new CoreCTMC[A] with CTMC[A]{
    override def nextWithRate(a: A) = f.applyOrElse(a,(x: A)=>Set[(Double,A)]())
  }

  def ofRelation[A](r: Set[(A,Double,A)]): CTMC[A] = ofFunction{ case a =>
    r filter {_._1 == a} map {t=>(t._2,t._3)}
  }

  def ofTransitions[A](r: (A,Double,A)*): CTMC[A] = ofRelation(r.toSet)
}