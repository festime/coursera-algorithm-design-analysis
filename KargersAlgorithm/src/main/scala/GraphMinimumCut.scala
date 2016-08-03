package graphminimumcut

import scala.io.Source
import collection.mutable.{ArrayBuffer}

class GraphMinimumCut(filename: String = "small_input.txt") {
  require (filename != "")

//    fileLines.foreach(println)
//    println(fileLines.length)
  val fileLines = Source
    .fromFile(filename)
    .getLines.toList.filter(s => s != "")
    .map(s => s.split(" ").map(n => n.toInt))
  var adjacencyList = graphData()
  var parentVertexId = collection.mutable.ArrayBuffer
    .range(0, adjacencyList.length)
  var remainingVertices = collection.mutable.Set[Int]()

  for (i <- 1 until adjacencyList.length) remainingVertices += i

  def mergeTwoVertices() = {
    val (parentVertex, childVertex) = randomEdge()

    removeEdge(parentVertex, childVertex)

    for (i <- 1 until parentVertexId.length) {
      if (parentVertexId(i) == childVertex) {
        parentVertexId(i) = parentVertex
      }

      if (adjacencyList(i).getOrElse(childVertex, 0) > 0) {
        if (adjacencyList(i).getOrElse(parentVertex, 0) > 0) {
          adjacencyList(i)(parentVertex) += adjacencyList(i)(childVertex)
          adjacencyList(parentVertex)(i) += adjacencyList(i)(childVertex)

        } else {
          adjacencyList(i)(parentVertex) = adjacencyList(i)(childVertex)
          adjacencyList(parentVertex)(i) = adjacencyList(i)(childVertex)
        }

        adjacencyList(i)(childVertex) = 0
        adjacencyList(childVertex)(i) = 0

      } else {
        // do nothing
      }
//      try {
//        if (adjacencyList(i)(childVertex) > 0) {
//          try {
//            adjacencyList(i)(parentVertex) += adjacencyList(i)(childVertex)
//            adjacencyList(parentVertex)(i) += adjacencyList(i)(childVertex)
//
//          } catch {
//            case e:java.util.NoSuchElementException => {
//              adjacencyList(i)(parentVertex) = adjacencyList(i)(childVertex)
//              adjacencyList(parentVertex)(i) = adjacencyList(i)(childVertex)
//            }
//          }
//
//          adjacencyList(i)(childVertex) = 0
//          adjacencyList(childVertex)(i) = 0
//        }
//
//      } catch {
//        case e:java.util.NoSuchElementException => {
//          // do nothing
//        }
//      }
    }
  }

  def removeEdge(parentVertex: Int, childVertex: Int) = {
    adjacencyList(parentVertex)(childVertex) = 0
    adjacencyList(childVertex)(parentVertex) = 0
    remainingVertices.remove(childVertex)
  }

  def randomEdge(): (Int, Int) = {
    val edges = collection.mutable.ArrayBuffer[(Int, Int)]()

    for (vertex <- remainingVertices.toIterator) {
      val remainingAdjacentVertices = adjacencyList(vertex).filter {
        case (adjacenctVertex, edgeNum) => edgeNum > 0
      }.keys
      
      for (adjacentVertex <- remainingAdjacentVertices.toIterator) {
        edges.append((vertex, adjacentVertex))
      }
    }
//    adjacencyList.zipWithIndex.foreach {
//      case(map, vertex) => {
//        map.foreach {
//          case (adjacentVertex, edgeNumber) => edges.append((vertex, adjacentVertex))
//        }
//      }
//    }

    val randomIndex = scala.util.Random.nextInt(edges.length)
    edges(randomIndex)
  }
  
//  def unfinished(): Boolean = remainingVertices.size > 2

  def minCut(): Int = {
    var min = -1

    for (i <- 0 until parentVertexId.length * parentVertexId.length) {
//      println(i)
      adjacencyList = graphData()
      parentVertexId = collection.mutable.ArrayBuffer
        .range(0, adjacencyList.length)
      remainingVertices = collection.mutable.Set[Int]()
    
      for (i <- 1 until adjacencyList.length) remainingVertices += i

//      while (unfinished) {
      while (remainingVertices.size > 2) {
        mergeTwoVertices()
      }
//      println(remainingVertices.size)
      
      val remainingVertex = remainingVertices.head
//      println(remainingVertices)
      var crossEdgesNum = 0
      for (j <- 1 until parentVertexId.length) {
        if (parentVertexId(j) == remainingVertex) {
          crossEdgesNum += adjacencyList(j).values.sum
//          println("j = " + j + ", adjacencyList(j).values.sum = " + adjacencyList(j).values.sum)
        }
      }

      if (crossEdgesNum < min || min == -1) {
        min = crossEdgesNum
      }
    }

    min
  }

  def graphData(): ArrayBuffer[collection.mutable.Map[Int, Int]] = {
    val edgeNumber = 1
    val arrayMutableMaps = ArrayBuffer[collection.mutable.Map[Int, Int]](collection.mutable.Map[Int, Int]())

    for (array <- fileLines) {
      val map = collection.mutable.Map[Int, Int]()

      for (i <- 1 until array.length) {
        map(array(i)) = 1
      }
      
      arrayMutableMaps.append(map)
    }
    
    arrayMutableMaps

//    collection.mutable.Map[Int, Int]() +:
//      (fileLines map {
//        array => collection.mutable.Map() ++ array.tail.map(n => n -> edgeNumber).toMap 
//      })
  }
}

object Main extends App {
  val g = new GraphMinimumCut
  
  println(g.minCut())
}