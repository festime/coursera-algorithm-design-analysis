package heapsort

import org.scalatest.FunSuite

class HeapSortSuite extends FunSuite {
  test("an increasing array should not be changed") {
    val heapSort = new HeapSort(Array(1, 2, 3, 4, 5))

    assert(heapSort.sort === Array(1, 2, 3, 4, 5))
  }

  test("a decreasing array should be reversed") {
    val heapSort = new HeapSort(Array(5, 4, 3, 2, 1))

    assert(heapSort.sort === Array(1, 2, 3, 4, 5))
  }

  test("an empty array should not be changed") {
    val heapSort = new HeapSort(Array())

    assert(heapSort.sort === Array())
  }

  test("") {
    val heapSort = new HeapSort(Array(1, 4, 5, 3, 2, 10, 7, 6, 9, 8))

    assert(heapSort.sort === (1 to 10).toArray)
  }
}