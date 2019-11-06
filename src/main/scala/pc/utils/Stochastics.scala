package pc.utils

import scala.util.Random

object Stochastics extends App {

  implicit lazy val random = new Random()

  // (p1,a1),...,(pn,an) --> (p1,a1),(p1+p2,a2),..,(p1+..+pn,an)
  def cumulative[A](l: List[(Double,A)]): List[(Double,A)] =
    l.tail.scanLeft(l.head){case ((r,_),(r2,a2)) => (r+r2,a2)}

  // (p1,a1),...,(pn,an) --> ai, selected randomly and fairly
  def draw[A](cumulativeList: List[(Double,A)])
             (implicit rnd: Random = random): A = {
    val rndVal = rnd.nextDouble() * cumulativeList.last._1
    cumulativeList.collectFirst{ case (r, a) if r >= rndVal => a }.get
  }

  // (p1,a1),...,(pn,an) + 100 --> {a1 -> P1%,...,an -> Pn%}
  def statistics[A](choices: Set[(Double,A)], size: Int)
                   (implicit rnd: Random = random): Map[A,Int] =
    (1 to size).map(i => Stochastics.draw(cumulative(choices.toList))(random))
                .groupBy(identity)
                .mapValues(_.size)
}
