package twosum

import collection.mutable.HashMap
import scala.io.Source
import scala.collection.Searching._

class TwoSum(inputFile: String) {
  val (array, hash) = {
    val temp = HashMap[Long, Boolean]()
    val a = collection.mutable.ArrayBuffer[Long]()

    for (line <- Source.fromFile(inputFile).getLines()) {
      val n = line.trim.toLong

      a += n
      temp(n) = true
    }

    println(a.length)
    (a.sorted, temp)
  }

  def countDistinctNumberPair(): Int = {
    val qualifiedPair = HashMap[Int, Boolean]()

    for (n <- array) {
      val arrayLength = array.length
      val lowerBound = -n - 10000
      val upperBound = -n + 10000
      val lowerBoundIndex = array.search(lowerBound).insertionPoint
      val upperBoundIndex = array.search(upperBound).insertionPoint

      for (i <- lowerBoundIndex to upperBoundIndex if i < arrayLength) {
        val sum = n + array(i)

        if (sum >= -10000 && sum <= 10000) {
          qualifiedPair(sum.toInt) = true
        }
      }
    }

    qualifiedPair.keys.size

//    val BreakValue = 10001
//    var count = 0
//
//    for (x <- array) {
//      var target = -10000
//
//      while (target <= 10000) {
//        val y = target - x
//
//        if (hash.getOrElse(y, false) && x != y) {
//          count += 1
//          target = BreakValue
//
//        } else {
//          target += 1
//        }
//      }
//    }
//
//    count
  }
}

object Main extends App {
//  println(new TwoSum("test_input.txt").countDistinctNumberPair())
  val temp = new TwoSum("test_input.txt")

  println(temp.countDistinctNumberPair())
//  val index = temp.array.search(0.toLong).insertionPoint
//  println(temp.array.slice(index - 50, index + 50))
}