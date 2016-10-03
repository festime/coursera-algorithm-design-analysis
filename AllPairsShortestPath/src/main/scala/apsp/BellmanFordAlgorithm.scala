package apsp

import scala.io.Source
import collection.mutable.{HashMap, ArrayBuffer}

class BellmanFordAlgorithm(inputFile: String) {
//  val PositiveInfinity = Int.MaxValue
  val PositiveInfinity = Double.PositiveInfinity
  val lineIterator = Source.fromFile(inputFile).getLines
  val (revGraph, numVertices) = {
    val (numVertices, numEdges) =
      lineIterator.next.split(" ") match {
        case Array(numVertices, numEdges) =>
          (numVertices.toInt, numEdges.toInt)
      }
    val revGraph: ArrayBuffer[HashMap[Int, Double]] =
      ArrayBuffer.fill(numVertices + 1)(HashMap[Int, Double]())

    for (line <- lineIterator) {
      val (tail, head, cost) = line.split(" ") map (_.toInt) match {
        case Array(tail, head, cost) => (tail, head, cost)
      }

      revGraph(head)(tail) = cost
    }

    assert((revGraph map (_.keys.size)).sum == numEdges)

    (revGraph, numVertices)
  }

  def run(): Option[Double] = {
    var previousShortestPathLength: ArrayBuffer[Double] =
      ArrayBuffer.fill(numVertices + 1)(0)
    var currentShortestPathLength: ArrayBuffer[Double] =
      ArrayBuffer.fill(numVertices + 1)(0)
    var minCost: Double = PositiveInfinity

    for (s <- 1 to numVertices) {
      for (i <- 0 to numVertices) {
        previousShortestPathLength(i) = PositiveInfinity
      }
      previousShortestPathLength(s) = 0

      /*
       * Find the length of shortest path from s to v with
       * fixed number of edges.
       */
      for (numEdges <- 1 to (numVertices - 1)) {
        findShortestPathLengthWithFixedNumEdges(
          previousShortestPathLength,
          currentShortestPathLength)

        val temp = previousShortestPathLength
        previousShortestPathLength = currentShortestPathLength
        currentShortestPathLength = temp
      }

      /*
       * check negative cycles
       */
      findShortestPathLengthWithFixedNumEdges(
        previousShortestPathLength,
        currentShortestPathLength)
      for (i <- 1 to numVertices) {
        if (previousShortestPathLength(i) != currentShortestPathLength(i)) {
          println("Negative cycle!")
          return None
        }
      }

      /*
       * If there is no negative cycle in graph,
       * compare and update minCost appropriately.
       */
      for (i <- 1 to numVertices) {
        if (minCost > currentShortestPathLength(i))
          minCost = currentShortestPathLength(i)
      }
    }

    println(minCost)
    return Some(minCost)
  }

  def findShortestPathLengthWithFixedNumEdges(
    previousShortestPathLength: ArrayBuffer[Double],
    currentShortestPathLength: ArrayBuffer[Double]): Unit = {

    for (v <- 1 to numVertices) {
      val choiceOne: Double =
        (revGraph(v) map {
          case (tail, cost) =>
            previousShortestPathLength(tail) + cost
//            if (previousShortestPathLength(v) == PositiveInfinity)
//              PositiveInfinity
//            else
//              if (previousShortestPathLength(tail) == PositiveInfinity)
//                PositiveInfinity
//              else
//                previousShortestPathLength(tail) + cost
        }).min
      val choiceTwo: Double = previousShortestPathLength(v)

      currentShortestPathLength(v) =
        if (choiceOne < choiceTwo)
          choiceOne
        else
          choiceTwo
    }
  }
}

object BellmanFordAlgorithm extends App {
  new BellmanFordAlgorithm("test_input_1.txt").run
  new BellmanFordAlgorithm("test_input_2.txt").run
  new BellmanFordAlgorithm("test_input_3.txt").run
}