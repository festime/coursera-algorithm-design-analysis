package dynamicprogramming

import scala.io.Source
import collection.mutable.ArrayBuffer

class KnapsackAlgorithm(inputFile: String) {
  case class Item(value: Int, weight: Int) {
    require(
      (value > 0 && weight > 0) || (value == 0 && weight == 0),
      "value = " + value + ", weight = " + weight)
  }
  val NilItem = Item(0, 0)

  val (knapsackSize, items, numberOfItems) = {
    val lineIterator = Source.fromFile(inputFile).getLines
    val temp = lineIterator.next.split(" ")
    val knapsackSize: Int = temp(0).toInt
    val numberOfItems: Int = temp(1).toInt
    val items = ArrayBuffer[Item](NilItem)

    for (line <- lineIterator) {
      val valueAndWeight = line.split(" ")
      val value = valueAndWeight(0).toInt
      val weight = valueAndWeight(1).toInt

      items += Item(value, weight)
    }

    (knapsackSize, items, numberOfItems)
  }

  def run: Int = {
    val values =
      ArrayBuffer[ArrayBuffer[Int]](ArrayBuffer.fill(knapsackSize + 1)(0))

    for (n <- 1 to numberOfItems) {
      val valuesWhenFrontNItemsAvailable =
        ArrayBuffer.fill(knapsackSize + 1)(0)

      for (weightUpperBound <- 1 to knapsackSize) {
        val solutionNotContainNthItem: Int = values(n - 1)(weightUpperBound)
        val solutionContainNthItem: Int = {
          val weightOfNthItem: Int = items(n).weight

          if (weightUpperBound - weightOfNthItem >= 0) {
            values(n - 1)(weightUpperBound - weightOfNthItem) + items(n).value

          } else {
            0
          }
        }

        valuesWhenFrontNItemsAvailable(weightUpperBound) =
          if (solutionNotContainNthItem > solutionContainNthItem)
            solutionNotContainNthItem
          else
            solutionContainNthItem
      }

      values += valuesWhenFrontNItemsAvailable
    }

    values.last.last
  }

  def runWithLessMemory: Long = {
    var values = ArrayBuffer.fill(knapsackSize + 1)(0L)

    for (n <- 1 to numberOfItems) {
      val valuesWhenFrontNItemsAvailable =
        ArrayBuffer.fill(knapsackSize + 1)(0L)

      for (weightUpperBound <- 1 to knapsackSize) {
        val solutionNotContainNthItem: Long = values(weightUpperBound)
        val solutionContainNthItem: Long = {
          val weightOfNthItem: Int = items(n).weight

          if (weightUpperBound - weightOfNthItem >= 0) {
            values(weightUpperBound - weightOfNthItem) + items(n).value

          } else {
            0
          }
        }

        valuesWhenFrontNItemsAvailable(weightUpperBound) =
          if (solutionNotContainNthItem > solutionContainNthItem)
            solutionNotContainNthItem
          else
            solutionContainNthItem
      }

      values = valuesWhenFrontNItemsAvailable
    }

    values.last
  }
}

object Main extends App {
  println(new KnapsackAlgorithm("test_input.txt").run)
  println(new KnapsackAlgorithm("big_test_input.txt").runWithLessMemory)
}