package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // insert element into empty heap => findMin returns same element
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min insert") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // insert 2 elements, find min of the 2 => findMin returns smaller element
  property("min of 2 elems") = forAll { (v1: A, v2: A) =>
    val h = insert(v1, insert(v2, empty))
    findMin(h) == min(v1,v2)
  }

  // Given any heap, you should get a sorted sequence of elements when continually
  // finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("recursive delete") = forAll { (h: H) =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val h1 = deleteMin(h)
        isEmpty(h1) || ((findMin(h) <= findMin(h1)) && isSorted(h1))
      } 
    }
    isSorted(h)
  }

  // Finding a minimum of the melding of any two heaps should
  // return a minimum of one or the other.
  property("min meld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1,h2)) == min(findMin(h1), findMin(h2))
  }

  property("meld1") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else meld(h, empty) == h
  }

}
