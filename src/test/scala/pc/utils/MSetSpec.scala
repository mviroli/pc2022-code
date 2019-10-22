package pc.utils

import org.scalatest.FlatSpec

class MSetSpec extends FlatSpec{

  "An empty MSet" should "have size 0" in {
    assert( MSet[Int]().size == 0)
  }

  "A MSet" should "be equal to another with just different ordering of elements" in {
    assert( MSet(10,20,30,30,15,15) == MSet(10,20,30,15,30,15) )
  }

  "A MSet" should "not be equal to another when adding s repetition" in {
    assert( MSet(10,20,30,30,15,15) != MSet(10,20,30,15,30,15,5,5) )
  }

  "A MSet" should "be equally constructed as List or as Map" in {
    assert( MSet(10,20,30,30,15,15) == MSet.ofMap(Map(10->1,20->1,30->2,15->2)) )
  }


}
