package heapsort

import heap._

class HeapSort(input: Array[Int]) {
  private val heap = {
    val inputHeap = new Heap[Int]

    for (i <- input) inputHeap.insert(i)

    inputHeap
  }

  def sort(): Array[Int] = {
    if (heap.nonEmpty) {
      Array(heap.extractMin) ++ sort()

    } else {
      Array[Int]()
    }
  }
}

object Main extends App {
  println((new HeapSort(Array(1, 2, 3, 4, 5))).sort.mkString(" "))
}