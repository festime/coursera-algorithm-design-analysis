package mst

import scala.io.Source
import collection.mutable.{ArrayBuffer, HashMap}

class Edge(val u: Int, val v: Int, val cost: Int) extends Ordered[Edge] {
  override def toString: String = u + " -> " + v + " $" + cost

  def compare(that: Edge): Int = this.cost compare that.cost
}

class PrimAlgorithm(inputFile: String) {
  val explored = HashMap[Int, Boolean]()
  val (adjacencyList, numberOfVertices, numberOfEdges) = {
    val adjacencyList = HashMap[Int, ArrayBuffer[Edge]]()
    val lineIterator = Source.fromFile(inputFile).getLines
    val (numberOfVertices, numberOfEdges) = {
      lineIterator.next.split(" ") match {
        case Array(numberOfVertices, numberOfEdges) =>
          (numberOfVertices.toInt, numberOfEdges.toInt)
      }
    }
    val createEdge = (head: Int, tail: Int, cost: Int) => {
      val newEdge = new Edge(head, tail, cost)

      adjacencyList.get(head) match {
        case Some(a: ArrayBuffer[Edge]) => a += newEdge
        case None => adjacencyList(head) = ArrayBuffer[Edge](newEdge)
      }
    }

    for (line <- lineIterator) {
      val (u, v, cost) = line.split(" ") match {
        case Array(u, v, cost) => (u.toInt, v.toInt, cost.toInt)
      }

      createEdge(u, v, cost)
      createEdge(v, u, cost)
      explored(u) = false
      explored(v) = false
    }

    (adjacencyList, numberOfVertices, numberOfEdges)
  }

  assert(numberOfVertices == 500)
  assert(numberOfEdges == 2184, numberOfEdges)

  def run(): Int = {
    for ((u, a) <- adjacencyList) {
      for (edge <- a)
        assert(edge.u == u)
      assert(explored(u) == false)
    }
    val s = explored.keys.head
    val exploredVertices = ArrayBuffer[Int](s)
    val minimumSpanningTree = ArrayBuffer[Edge]()
    var totalCost = 0

    explored(s) = true

    while (exploredVertices.length < numberOfVertices) {
      var minCostEdge = new Edge(0, 0, Int.MaxValue)

      val frontierEdges = exploredVertices.flatMap {
        vertex => adjacencyList(vertex)
      }

      var i = 0
      while (i < exploredVertices.length) {
        val u = exploredVertices(i)
        var j = 0

        while (j < adjacencyList(u).length) {
          val edge = adjacencyList(u)(j)

          if (minCostEdge.cost > edge.cost && explored(edge.v) == false)
            minCostEdge = edge

          j += 1
        }
        i += 1
      }

      assert(minCostEdge.u != minCostEdge.v, minCostEdge)
      assert(explored(minCostEdge.u), minCostEdge)
      assert(!explored(minCostEdge.v), minCostEdge)

      if (minCostEdge.u != minCostEdge.v) {
        explored(minCostEdge.v) = true
        exploredVertices += minCostEdge.v
        minimumSpanningTree += minCostEdge
        totalCost += minCostEdge.cost
      }
    }

    assert(minimumSpanningTree.length == numberOfVertices - 1)

    totalCost
  }
}

object Main extends App {
  println(new PrimAlgorithm("test_input.txt").run)
}