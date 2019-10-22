package pc.modelling

import org.scalatest.FlatSpec

class PNSpec extends FlatSpec{

  import pc.examples.PNMutualExclusion, pc.examples.PNMutualExclusion.place._
  import pc.utils.MSet

  val pnME = PNMutualExclusion.mutualExclusionSystem()

  "PN for mutual exclusion" should "properly generate 7-length paths" in {

    val expected1 = List(MSet(N,N), MSet(T,N), MSet(T,T), MSet(C,T), MSet(T), MSet(C), MSet())
    val expected2 = List(MSet(N,N), MSet(T,N), MSet(C,N), MSet(C,T), MSet(T), MSet(C), MSet())
    val expected3 = List(MSet(N,N), MSet(T,N), MSet(C,N), MSet(N), MSet(T), MSet(C), MSet())

    assert(pnME.paths(MSet(N,N),7).toSet == Set(expected1,expected2,expected3))
  }
}
