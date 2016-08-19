package shortest.distance

import scala.io.Source
import collection.mutable.ArrayBuffer
import heap._

class Graph(inputFile: String = "test_input.txt") {
  val (innerRepresentation, numberOfVertices) = createGraph(inputFile)

  def adjacentVerticesAt(i: Int) = innerRepresentation(i)

  private def createGraph(inputFile: String) = {
    val N = 201
    var n = 0
    val graph = Array.fill(N) { collection.mutable.Map[Int, Int]() }

    for (line <- Source.fromFile(inputFile).getLines()) {
      val parsedLine = line.split("\t")
      val i = parsedLine(0).toInt

      for (pairStr <- parsedLine.tail) {
        val adjacentVertex = pairStr.split(",")(0).toInt
        val edgeLength = pairStr.split(",")(1).toInt

        graph(i)(adjacentVertex) = edgeLength
      }

      n += 1
    }

    (graph, n)
  }
}

class DijkstraAlgorithm(inputFile: String = "input_text.txt") {
  val graph = new Graph(inputFile)
//  val shortestDistances = run
  val shortestDistances = runWithHeap

  class Vertex(val value: Int, val score: Int = 0) extends Ordered[Vertex] {
    def compare(that: Vertex): Int = {
      this.score compare that.score
    }

    override def hashCode = value

    override def equals(that: Any): Boolean = that match {
      case that: Vertex => this.value == that.value
      case _ => false
    }

    override def toString() = value.toString + "@" + score.toString
  }

  private def runWithHeap = {
    val numberOfVertices = graph.numberOfVertices
    val source = 1
    val processedVertices = ArrayBuffer[Int](source)
    val unprocessedVerticesHeap = new Heap[Vertex]
    val CannotReach = 1000000
    val shortestDistanceFromSourceTo = ArrayBuffer
      .fill(numberOfVertices + 1) { CannotReach }

    shortestDistanceFromSourceTo(source) = 0

    for ((vertex, edgeLength) <- graph.adjacentVerticesAt(source)) {
      unprocessedVerticesHeap.insert(new Vertex(vertex, edgeLength))
    }

    while (unprocessedVerticesHeap.toArrayBuffer.nonEmpty) {
      val minScoreVertex = unprocessedVerticesHeap.extractMin()

      for ((adjacentVertex, edgeLength) <- graph.adjacentVerticesAt(minScoreVertex.value)) {
        if (processedVertices.contains(adjacentVertex) == false) {
          val option = unprocessedVerticesHeap.delete(new Vertex(adjacentVertex))

          if (option != None) {
            if (option.get.score > minScoreVertex.score + edgeLength) {
              unprocessedVerticesHeap
                .insert(new Vertex(adjacentVertex, minScoreVertex.score + edgeLength))

            } else {
              unprocessedVerticesHeap.insert(option.get)
            }

          } else {
            unprocessedVerticesHeap.insert(new Vertex(adjacentVertex, minScoreVertex.score + edgeLength))
          }
        }
      }

      processedVertices += minScoreVertex.value
      shortestDistanceFromSourceTo(minScoreVertex.value) = minScoreVertex.score
    }

    shortestDistanceFromSourceTo
  }

  private def run = {
    val numberOfVertices = graph.numberOfVertices
    val source = 1
    val processedVertices = ArrayBuffer[Int](source)
    val unprocessedVertices = collection
      .mutable
      .Set[Int]((2 to numberOfVertices).toList : _*)
    val CannotReach = 1000000
    val shortestDistanceFromSourceTo = ArrayBuffer
      .fill(numberOfVertices + 1) { CannotReach }

    shortestDistanceFromSourceTo(source) = 0

    while (unprocessedVertices.nonEmpty) {
      var minGreedyScore = Int.MaxValue
      var vertexWithMinScore = 0
      var shortestDistance = 0

      for (i <- processedVertices) {
        for ((adjacentVertex, edgeLength) <- graph.adjacentVerticesAt(i)) {
          if (unprocessedVertices.contains(adjacentVertex)) {
            val pathLength = shortestDistanceFromSourceTo(i) + edgeLength

            if (pathLength < minGreedyScore) {
              minGreedyScore = pathLength
              vertexWithMinScore = adjacentVertex
              shortestDistance = pathLength
            }
          }
        }
      }

      shortestDistanceFromSourceTo(vertexWithMinScore) = shortestDistance
      unprocessedVertices.remove(vertexWithMinScore)
      processedVertices += vertexWithMinScore
    }

//    for ((distance, i) <- shortestDistanceFromSourceTo.view.zipWithIndex) {
//      println(i + ": " + distance)
//    }

    shortestDistanceFromSourceTo
  }
}

object Main extends App {
//  val algorithm = new DijkstraAlgorithm("test_input.txt")
//  val indices = Array(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
//
//  val output = indices.map {
//    n => algorithm.shortestDistances(n)
//  }.mkString(",")
//  println(output)

  val inputFile = "test_input.txt"
  for (line <- Source.fromFile(inputFile).getLines()) {
    val parsedLine = line.split("\t")
    val i = parsedLine(0).toInt

    for (pairStr <- parsedLine.tail) {
      val adjacentVertex = pairStr.split(",")(0).toInt
      val edgeLength = pairStr.split(",")(1).toInt

      println("\t" + adjacentVertex + ", " + edgeLength)
    }
    parsedLine.foreach((s: String) => print(s + " "))
    println("")
  }
}