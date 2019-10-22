package pc.modelling

import org.scalatest.FlatSpec
import pc.utils.MSet

class PNSpec extends FlatSpec{
  "PN for mutual exclusion" should "properly generate 7-length paths" in {
    // Specification of my data-type for states
    object place extends Enumeration {
      val n,t,c = Value
    }
    type Place = place.Value
    import PetriNet._
    import place._

    val pn = PetriNet[Place](
      MSet(n) ~~> MSet(t),
      MSet(t) ~~> MSet(c) ^^^ MSet(c),
      MSet(c) ~~> MSet())

    val system = toSystem(pn)

    val expected1 = List(MSet(n,n), MSet(t,n), MSet(t,t), MSet(c,t), MSet(t), MSet(c), MSet())
    val expected2 = List(MSet(n,n), MSet(t,n), MSet(c,n), MSet(c,t), MSet(t), MSet(c), MSet())
    val expected3 = List(MSet(n,n), MSet(t,n), MSet(c,n), MSet(n), MSet(t), MSet(c), MSet())

    assert(system.paths(MSet(n,n),7).toSet == Set(expected1,expected2,expected3))
  }
}
