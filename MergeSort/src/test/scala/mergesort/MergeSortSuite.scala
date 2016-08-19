package mergesort

import org.scalatest.FunSuite

class MergeSortSuite extends FunSuite {
  test("an increasing array should not be changed") {
    assert(MergeSort.sort(Array(1, 2, 3, 4, 5)) === Array(1, 2, 3, 4, 5))
  }

  test("a decreasing array should be reversed") {
    assert(MergeSort.sort(Array(5, 4, 3, 2, 1)) === Array(1, 2, 3, 4, 5))
  }

  test("an empty array should not be changed") {
    assert(MergeSort.sort(Array()) === Array())
  }

  test("") {
    assert(MergeSort.sort(Array(1, 4, 5, 3, 2, 10, 7, 6, 9, 8)) === (1 to 10).toArray)
  }
}