package median

import scala.io.Source
import collection.mutable.{ArrayBuffer, PriorityQueue}

class MedianMaintenance(inputFile: String) {
  val array = {
    val temp = ArrayBuffer[Int]()

    for (line <- Source.fromFile(inputFile).getLines()) {
      val n = line.trim.toInt

      temp += n
    }

    temp
  }
  val medians = {
    val smallerHalf = PriorityQueue[Int]()
    val largerHalf = PriorityQueue[Int]()(Ordering[Int].reverse)
    val medians = ArrayBuffer[Int](0, array(0))

    if (array(0) > array(1)) {
      smallerHalf.enqueue(array(1))
      largerHalf.enqueue(array(0))

    } else {
      smallerHalf.enqueue(array(0))
      largerHalf.enqueue(array(1))
    }

    medians += smallerHalf.head

    var size = 2
    for (n <- array.drop(2)) {
      assert(smallerHalf.head <= largerHalf.head)
      if (size % 2 == 0) {
        assert(smallerHalf.size == size / 2 && largerHalf.size == size / 2,
               "smallerHalf.size = " + smallerHalf.size + ", largerHalf.size = " + largerHalf.size)
      } else {
        assert(smallerHalf.size == size / 2 + 1 && largerHalf.size == size / 2)
      }

      if (n <= largerHalf.head) {
        smallerHalf.enqueue(n)

      } else {
        largerHalf.enqueue(n)
      }

      if (smallerHalf.size - largerHalf.size > 1) {
        val v = smallerHalf.dequeue
        largerHalf.enqueue(v)
        
      } else if (largerHalf.size > smallerHalf.size) {
        val v = largerHalf.dequeue
        smallerHalf.enqueue(v)
      }

      size += 1
      medians += smallerHalf.head
    }

    medians
  }

  def mediansSumMod = medians.sum % 10000
}

object Main extends App {
  val mm = new MedianMaintenance("median_test_input.txt")
  
  println(mm.medians.length)
  println(mm.mediansSumMod)
}