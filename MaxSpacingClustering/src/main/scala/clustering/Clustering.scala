package clustering

import collection.mutable.ArrayBuffer
import scala.io.Source
import heap._

/*
 * Clustering, aka "unsupervised learning"
 * Given n `points`, classify into `coherent groups`.
 */

class UnionFind(n: Int) {
  private val leader: ArrayBuffer[Int] = ArrayBuffer((0 to n): _*)
  private val size = ArrayBuffer.fill(n + 1)(1)
  private var number = n
  size(0) = 0

  /*
   * O(n) running time, where n is the number of elements in union-find
   */
  def union(x: Int, y: Int): (Int, IndexedSeq[Int]) = {
    require(x >= 1 && x <= n)
    require(y >= 1 && y <= n)
    require(leader(x) != leader(y), "x = " + x + ", y = " + y + ", leader(x) = " + leader(x) + ", leader(y) = " + leader(y))

    val oldSize = leader.toSet.size
    val (smallerGroupLeader, largerGroupLeader) =
      if (size(x) < size(y)) (leader(x), leader(y))
      else (leader(y), leader(x))

    size(largerGroupLeader) += size(smallerGroupLeader)
    val smallerGroup =
      for (i <- 1 to n if leader(i) == smallerGroupLeader)
        yield(i)
    for (i <- smallerGroup) {
      leader(i) = largerGroupLeader
      size(i) = 0
    }

    assert(size.sum == n, size.sum)
    val newSize = leader.toSet.size
    for (i <- 1 to n) assert(leader(i) != smallerGroupLeader, "i = " + i + ", leader(" + i + ") = " + leader(i))
    assert(newSize == oldSize - 1, "newSize = " + newSize + ", oldSize = " + oldSize)

    number -= 1

    (smallerGroupLeader, smallerGroup)
  }

  /*
   * O(1) running time
   */
  def find(x: Int): Int = leader(x)

  /*
   * O(1) running time
   */
  def merged(x: Int, y: Int): Boolean = leader(x) == leader(y)

  /*
   * O(1) running time
   */
  def numberOfGroups: Int = number

  def groups: List[Int] = leader.toList.sortWith (_ > _).distinct
}

class SeparatedTwoPoints(val u: Int, val v: Int, val distance: Int) extends Ordered[SeparatedTwoPoints] {
  // require that u and v are separated

  require(u < v, "u = " + u + ", v = " + v)

  override def toString = u.toString + " -> " + v.toString + " = " + distance.toString

  /*
   * SeparatedTwoPoints 物件要被放在 heap 裡
   * 在此演算法裡要用兩個 points 之間的 distance 比大小
   */
  def compare(that: SeparatedTwoPoints): Int =
    this.distance compare that.distance

  override def hashCode: Int = (u.toString + "000" + v.toString).toInt

  /*
   * 定義 equals 才能使 heap 正確刪除
   * 我們定義上邏輯相等的物件
   */
  override def equals(that: Any) = that match {
    case that: SeparatedTwoPoints => this.u == that.u && this.v == that.v
    case _ => false
  }
}

class MaxSpacingClusteringAlgorithm(inputFile: String) {
  /*
   * 最吃時間的在 heap 的初始化
   * 接近 n ^ 2 筆資料插入所需的時間
   * = O((n ^ 2) log(n ^ 2)) = O((n ^ 2) log n)
   * where n is the number of vertices
   */
  val (heap, clusters, numberOfVertices) = {
    val lineIterator = Source.fromFile(inputFile).getLines
    val numberOfVertices: Int = lineIterator.next.toInt
    val heap = new Heap[SeparatedTwoPoints]()

    for (line <- lineIterator) {
      val (u, v, distance) = line.split(" ") match {
        case Array(u, v, distance) => (u.toInt, v.toInt, distance.toInt)
      }

      heap.insert(new SeparatedTwoPoints(u, v, distance))
    }

    assert(heap.size == numberOfVertices * (numberOfVertices - 1) / 2)

    (heap, new UnionFind(numberOfVertices), numberOfVertices)
  }

  /*
   * calculate the spacing of a k-clustering
   * i.e., min_separated p, q d(p, q)
   */
  def run(targetNumberOfClusters: Int) = {
    while (clusters.numberOfGroups > targetNumberOfClusters) {
      val separatedTwoPoints = heap.extractMin
      val (u, v) = (separatedTwoPoints.u, separatedTwoPoints.v)
      val (smallerGroupLeader, smallerGroup) = clusters.union(u, v)

      for (i <- smallerGroup) {
        for (j <- 1 to numberOfVertices) {
          j match {
            case u if (u < i) =>
              if (clusters.merged(u, i))
                heap.delete(new SeparatedTwoPoints(u, i, 0))
            case v if (v > i) =>
              if (clusters.merged(i, v))
                heap.delete(new SeparatedTwoPoints(i, v, 0))
            case _ =>
          }
        }
      }
    }

    heap.extractMin.distance
  }
}

object Clustering extends App {
  println(new MaxSpacingClusteringAlgorithm("test_input.txt").run(4))
}