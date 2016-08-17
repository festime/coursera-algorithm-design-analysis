package heap

import org.scalatest.FunSuite
import collection.mutable.ArrayBuffer

class HeapSuite extends FunSuite {
  test("continuous extractMin of a heap consists of 1 to 5 should return 1 to 5 sequentially") {
    val heap = new Heap[Int]

    for (i <- 5 to 1 by -1) heap.insert(i)

    assert(heap.extractMin() === 1)
    assert(heap.extractMin() === 2)
    assert(heap.extractMin() === 3)
    assert(heap.extractMin() === 4)
    assert(heap.extractMin() === 5)
    intercept[java.lang.IndexOutOfBoundsException](heap.extractMin())
  }

  test("the smaller one of new node and original last node should bubble up") {
    val heap = new Heap[Int]

    heap.insert(2)
    heap.insert(1)
    heap.insert(3)

    assert(heap.toArrayBuffer === ArrayBuffer(1, 2, 3))
  }





  test("the smaller one of remaining nodes should bubble up") {
    val heap = new Heap[Int]

    heap.insert(1)
    heap.insert(2)
    heap.insert(3)
    heap.delete(1)

    assert(heap.toArrayBuffer === ArrayBuffer(2, 3))
  }

  test("bubble up correctness in delete process") {
    val heap = new Heap[Int]
    val input = List(1, 90, 2, 91, 92, 3, 4, 93, 94, 95, 96, 5)
    val output = ArrayBuffer(1, 5, 2, 90, 92, 3, 4, 93, 94, 95, 96)

    for (i <- input) heap.insert(i)
    heap.delete(91)

    assert(heap.toArrayBuffer === output)
  }

  test("bubble down correctness in delete process") {
    val heap = new Heap[Int]
    val input = List(1, 90, 2, 91, 92, 3, 4, 93, 94, 95, 96)
    val output = ArrayBuffer(1, 91, 2, 93, 92, 3, 4, 96, 94, 95)

    for (i <- input) heap.insert(i)
    heap.delete(90)

    assert(heap.toArrayBuffer === output)
  }

  test("delete last element in array representation should execute normally") {
    val heap = new Heap[Int]

    heap.insert(1)
    heap.insert(2)
    heap.delete(2)
  }
}