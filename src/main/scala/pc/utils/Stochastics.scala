package pc.utils

import scala.util.Random

object Stochastics {
  implicit val random = new Random()
  def draw[A](possibilities: Set[(Double,A)])(implicit rnd: Random = random): A = {
    val next = possibilities.toList.scanLeft[(Double,Option[A]),List[(Double,Option[A])]]((0.0, None)) {case ((r,_),(r2,a2)) => (r+r2,Some(a2))}
    val rnd1 = rnd.nextDouble() * next.last._1
    next.collectFirst{ case (r, Some(a)) if r >= rnd1 => a }.get
  }
}
