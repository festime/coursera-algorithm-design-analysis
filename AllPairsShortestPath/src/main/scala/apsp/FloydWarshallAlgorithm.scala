package apsp

import scala.io.Source
import collection.mutable.{HashMap, ArrayBuffer}

class FloydWarshallAlgorithm(inputFile: String) {
  val PositiveInfinity = Int.MaxValue
  val (graph, numVertices, numEdges, initialCost) = {
    val lineIterator = Source.fromFile(inputFile).getLines
    val (numVertices, numEdges) =
      lineIterator.next.split(" ") match {
        case Array(numVertices, numEdges) =>
          (numVertices.toInt, numEdges.toInt)
      }
    val N = numVertices + 1
    val graph = ArrayBuffer.fill(N)(HashMap[Int, Int]())
    val initialCost =
      ArrayBuffer
        .fill(N)(ArrayBuffer.fill(N)(PositiveInfinity))

    for (line <- lineIterator) {
      val (tail, head, cost) = line.split(" ") map (_.toInt) match {
        case Array(tail, head, cost) => (tail, head, cost)
      }

      graph(tail)(head) = cost
      initialCost(tail)(head) = cost
    }

    for (i <- 1 to numVertices) initialCost(i)(i) = 0

    // assert that number of edges is correct
    assert((graph map (_.keys.size)).sum == numEdges)
    // assert that initialCost is correct
    for (i <- 1 to numVertices) {
      for (j <- 1 to numVertices) {
        initialCost(i)(j) match {
          case PositiveInfinity => assert(graph(i).get(j) == None && i != j)
          case 0 => assert(i == j || graph(i)(j) == 0)
          case _ => assert(graph(i).get(j) != None)
        }
      }
    }

    (graph, numVertices, numEdges, initialCost)
  }
  val N = numVertices + 1

  def run(): Option[Long] = {
    var a = initialCost
    var b = ArrayBuffer.fill(N)(ArrayBuffer.fill(N)(0))

    for (k <- 1 to numVertices) {
      for (i <- 1 to numVertices) {
        for (j <- 1 to numVertices) {
          val costOne = a(i)(j)
          val costTwo =
            if (a(i)(k) == PositiveInfinity || a(k)(j) == PositiveInfinity)
              PositiveInfinity
            else
              a(i)(k) + a(k)(j)

          b(i)(j) =
            if (costOne < costTwo)
              costOne
            else
              costTwo
        }
      }

      /*
       * 重複利用陣列，避免額外的建立和 GC 開銷
       */
      var temp = b
      b = a
      a = temp
    }

    /*
     * 檢查是否有 negative cycle
     */
    for (i <- 1 to numVertices) {
      if (a(i)(i) < 0) {
        println("Negative cycle!")
        return None
      }
    }

    /*
     * n 表示圖的 vertices 數量
     * 元素有 n ^ 2 個，排序的時間複雜度應該是 O(n ^ 2 (log n)) 才對
     * 比 O(n ^ 2) 還大！
     * 下面這註解是錯的！！！！！！！！
     *
     *
     *
     * 找最短的「兩點間最短路徑」長度
     * 先把最終結果 flatten 再排序，最前面的就是最小的
     * 比起迭代兩層迴圈找最小值
     * 算是一個小小優化吧
     *
     * 時間複雜度從 O(n ^ 2) 變成 O(n log n)
     * 再來，這樣避免了 branch prediction 失敗對效能的影響
     *
     * 不過還是要再強調一次
     * 這只是小小優化
     * 因為上面的過程就是 O(n ^ 3) 了，主要時間花在這上面
     * 而且若圖有 negative cycle
     * 也不會執行到這裡
     */
     // val minCost = a.flatten.sorted.head

    var minCost = PositiveInfinity
    for (i <- 1 to numVertices) {
      for (j <- 1 to numVertices) {
        if (a(i)(j) < minCost)
          minCost = a(i)(j)
      }
    }

    println(minCost)
    return Some(minCost)
  }
}

object FloydWarshallAlgorithm extends App {
  new FloydWarshallAlgorithm("test_input_1.txt").run
  new FloydWarshallAlgorithm("test_input_2.txt").run
  new FloydWarshallAlgorithm("test_input_3.txt").run
}