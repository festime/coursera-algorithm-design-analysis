package heap

import org.scalatest.FunSuite

class HeapSuite extends FunSuite {
  test("min-heap extractMin correctness") {
    val heap = new Heap

    for (i <- 5 to 1 by -1) heap.insert(i)

    assert(heap.extractMin() === 1)
    assert(heap.extractMin() === 2)
    assert(heap.extractMin() === 3)
    assert(heap.extractMin() === 4)
    assert(heap.extractMin() === 5)
    intercept[java.lang.IndexOutOfBoundsException](heap.extractMin())
  }

  test("min-heap insert correctness") {
    val heap = new Heap

    for (i <- 5 to 1 by -1) heap.insert(i)

    assert(
      heap
        .content
        .sameElements(collection.mutable.ArrayBuffer[Int](1, 2, 4, 5, 3))
    )
  }

  test("min-heap delete correctness") {
    val heap = new Heap

    for (i <- 5 to 1 by -1) heap.insert(i)

    assert(heap.delete(5) === Some(5))
    assert(heap.content.contains(5) === false)
    assert(heap.delete(6) === None)
    assert(heap.delete(1) === Some(1))
    assert(heap.content.contains(1) === false)
  }
}