package scc

import scala.io.Source
import collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object DFS extends Exception { }

class Graph(inputFile: String = "test_input.txt") {
  val (normalG, reverseG, max) = createGraph()
  val finishTime = dfsOnReverseGraph()
  val sccCount = countStrongConnectedComponents()

  private def countStrongConnectedComponents() = {
    var exploration = ArrayBuffer.fill(max + 1) { false }
    val dfsStack = collection.mutable.Stack[Int]()
    val scc = ArrayBuffer[(Int, ArrayBuffer[Int])]()

    while (finishTime.nonEmpty) {
      val v = finishTime.pop

      if (exploration(v) == false) {
        scc += ((v, ArrayBuffer[Int]()))
        dfsStack.push(v)

        while (dfsStack.nonEmpty) {
          val currentVertex = dfsStack.head
          exploration(currentVertex) = true

          val unexploredAdjacentVertex = normalG(currentVertex)
            .find(((adjacentVertex: Int) => exploration(adjacentVertex) == false))
            .getOrElse(-1)

          if (unexploredAdjacentVertex == -1) {
            scc.last._2 += dfsStack.pop

          } else {
            dfsStack.push(unexploredAdjacentVertex)
          }

//          try {
//            for (adjacentVertex <- normalG.getOrElse(currentVertex, ArrayBuffer[Int]())) {
//            for (adjacentVertex <- normalG(currentVertex)) {
//              if (exploration(adjacentVertex) == false) {
//                dfsStack.push(adjacentVertex)
////                throw DFS
//              }
//            }

//          } catch {
//            case DFS =>
//          }
        }
      }
    }

    val result = scc.sortWith(_._2.length > _._2.length).take(5) map {
      case (leader, group) => group.length
    }
    
    if (result.length == 5) {
      result.mkString(",")

    } else {
      (result ++ Array.fill(5 - result.length) { 0 }).mkString(",")
    }
  }

  private def dfsOnReverseGraph() = {
    val stack = collection.mutable.Stack[Int]()
    val finishTimeStack = collection.mutable.Stack[Int]()
    var exploration = ArrayBuffer.fill(max + 1) { false }

    for (i <- max to 1 by -1) {
      if (exploration(i) == false) {
        stack.push(i)

        while (stack.nonEmpty) {
          val currentVertex = stack.head
          exploration(currentVertex) = true

          val unexploredAdjacentVertex = reverseG(currentVertex)
            .find(((adjacentVertex: Int) => exploration(adjacentVertex) == false))
            .getOrElse(-1)

          if (unexploredAdjacentVertex == -1) {
            finishTimeStack.push(currentVertex)
            stack.pop

          } else {
            stack.push(unexploredAdjacentVertex)
          }

//          try {
////            for (adjacentVertex <- reverseG.getOrElse(currentVertex, ArrayBuffer[Int]())) {
//            for (adjacentVertex <- reverseG(currentVertex)) {
//              if (exploration(adjacentVertex) == false) {
//                stack.push(adjacentVertex)
//                throw DFS
//              }
//            }
//
//            finishTimeStack.push(currentVertex)
//            stack.pop
//
//          } catch {
//            case DFS =>
//          }
        }
      }
    }

    finishTimeStack

//
//    def dfs(i: Int) {
//      exploration(i) = true
//      
//      for (adjacentVertex <- reverseG(i)) {
//        if (exploration(adjacentVertex) == false) {
//          dfs(adjacentVertex)
//        }
//      }
//
//      time += 1
//      finishTime(i) = time
//    }
//
//    for (i <- max to 1 by -1) {
//      if (exploration(i) == false) {
//        dfs(i)
//      }
//    }
//
//    finishTime
  }

  private def createGraph() = {
//    val gInMap = collection.mutable.Map[Int, ArrayBuffer[Int]]()
//    val revGraphInMap = collection.mutable.Map[Int, ArrayBuffer[Int]]()
    val g = Array.fill(875715) { ArrayBuffer[Int]() }
    val revG = Array.fill(875715) { ArrayBuffer[Int]() }
    var maxVertex = 0

    for (line <- Source.fromFile(inputFile).getLines()) {
      val tailAndHead = line.split(" ")
      val (tail, head) = (tailAndHead(0).toInt, tailAndHead(1).toInt)
//      val adjacentVerticesInG = gInMap
//        .getOrElseUpdate(tail, ArrayBuffer[Int](head))
//      val adjacentVerticesInRevG = revGraphInMap
//        .getOrElseUpdate(head, ArrayBuffer[Int](tail))
      val adjacentVerticesInG = g(tail)
      val adjacentVerticesInRevG = revG(head)

//      if (adjacentVerticesInG(0) != head) {
//        adjacentVerticesInG += head
//      }
//      if (adjacentVerticesInRevG(0) != tail) {
//        adjacentVerticesInRevG += tail
//      }
      adjacentVerticesInG += head
      adjacentVerticesInRevG += tail
      if (tail > maxVertex) { maxVertex = tail }
      if (head > maxVertex) { maxVertex = head }
    }

//    (gInMap, revGraphInMap, maxVertex)
    (g, revG, maxVertex)
  }
}

object KosarajuAlgorithm extends App {
  val graph = new Graph("test_input.txt")
  println(graph.sccCount)
}
