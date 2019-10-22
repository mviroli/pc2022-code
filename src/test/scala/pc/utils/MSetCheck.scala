package pc.utils

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

object MSetCheck extends Properties("MSet") {

  implicit def msetArbitrary[A:Arbitrary]: Arbitrary[MSet[A]] =
    Arbitrary(arbitrary[List[A]] map (MSet.ofList(_)))

  property ("has constructors that are compatible") = forAll { (list: List[Int]) =>
    val m1 = MSet.ofList(list); m1 == MSet.ofMap(m1.asMap)
  }

  property ("is ordering independent") = forAll { (list: List[Int]) =>
    MSet.ofList(list) == MSet.ofList(scala.util.Random.shuffle(list))
  }

  property ("is duplicate dependent") = forAll { (list: List[Int], i:Int) =>
    MSet.ofList(i::list) != MSet.ofList(i::i::list)
  }

  property ("has union semantics that corresponds to List's") = forAll { (list1: List[Int], list2: List[Int]) =>
    MSet.ofList(list1 union list2) == MSet.ofList(list1).union(MSet.ofList(list2))
  }

  property ("has diff semantics that corresponds to List's") = forAll { (list1: List[Int], list2: List[Int]) =>
    MSet.ofList(list1 diff list2) == MSet.ofList(list1).diff(MSet.ofList(list2))
  }

  property ("has size with semantics that corresponds to List's") = forAll { (list1: List[Int]) =>
    MSet.ofList(list1).size == list1.size
  }

  property ("has matches that is coherent with diff") = forAll { (mset1: MSet[Int], mset2: MSet[Int]) =>
    mset1 matches (mset1 diff mset2)
  }

  property ("has extract that is coherent with union") = forAll { (mset1: MSet[Int], mset2: MSet[Int]) =>
    val mset3 = mset1 diff mset2; ((mset1 extract mset3).get.union(mset3) == mset1)
  }

  property ("has iterator that gives values in MSet") = forAll { (mset: MSet[Int]) =>
    mset.iterator forall (mset matches MSet(_))
  }

  property ("has iterator that does not miss values in MSet") = forAll { (list: List[Int]) =>
    val elements = MSet.ofList(list).iterator.toList; list forall (elements contains _)
  }

  property ("has matchIterator that gives precisely all elements") = forAll { (mset: MSet[Int]) =>
    mset.matchIterator.map(_._1).toList == mset.iterator.toList
  }

  property ("has iterate that correctly gives remainders") = forAll { (mset: MSet[Int]) =>
    mset.matchIterator forall {case (a,m) => MSet(a).union(m) == mset}
  }
}
