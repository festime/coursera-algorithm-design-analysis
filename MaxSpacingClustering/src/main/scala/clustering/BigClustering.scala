package clustering

import collection.mutable.{HashMap, ArrayBuffer}
import scala.io.Source
import unionfind._

class BigClusteringAlgorithm(inputFile: String) {
  val (input, numberOfBits) = {
    val arrayBuffer = ArrayBuffer[Int]()
    val lineIterator = Source.fromFile(inputFile).getLines
    val (numberOfVertices: Int, numberOfBits: Int) = {
      val line = lineIterator.next.split(" ")
      (line(0).toInt, line(1).toInt)
    }

    var i = 1
    for (line <- lineIterator) {
      val n = Integer.parseInt(line.replaceAll(" ", ""), 2)

      arrayBuffer += n
      i += 1
    }

    assert(arrayBuffer.length == numberOfVertices)

    (arrayBuffer.toArray, numberOfBits)
  }
  val clusters = new Union[Int](input)

  /*
   * 想法是把所有 Hamming distance 恰為 1 的 point pairs 融合
   * 接下來把所有 Hamming distance 恰為 2 的 point pairs 融合
   * 剩下來的 separated point pairs 的 Hamming distance
   * 就一定都大於等於 3 了
   */
  def run(): Int = {
    /*
     * 1 bit toggle
     * fuse all point pairs with exactly 1 bit different
     * i.e., all point pairs with Hamming distance 1
     *
     * 總迴圈數 = 20_0000 x 24 = 480_0000
     * 每輪迴圈內做的事情成本接近 O(1)
     * 比起暴力迭代 input 的所有 point pair 可能情況
     * 因為 input size = 20_0000
     * 所以 point pair 總共可能 10_0000 x 19_9999 = 199_9990_0000 種情況
     * 便宜太多太多了
     */
    for (int <- input) {
      for (numberBitShift <- 0 until numberOfBits) {
        val toggler = 1 << numberBitShift
        val shiftedInt = int ^ toggler

        clusters.find(shiftedInt) match {
          case None =>
          case Some(n: Int) => clusters.union(int, shiftedInt)
        }
      }
    }

    /*
     * 2 bits toggle
     * fuse all point pairs with exactly 2 bits different
     * i.e., all point pairs with Hamming distance 2
     *
     * 總迴圈數 = 20_0000 x 276 = 5520_0000
     * 比起所有可能的 point pair 情況有 199_9990_0000 種
     * 差了三百多倍，還是非常非常划算的
     */
    for (int <- input) {
      for (i <- 0 until numberOfBits; j <- (i + 1) until numberOfBits) {
        val togglerOne = 1 << i
        val togglerTwo = 1 << j
        val shiftedInt = (int ^ togglerOne) ^ togglerTwo

        clusters.find(shiftedInt) match {
          case None =>
          case Some(n: Int) => clusters.union(int, shiftedInt)
        }
      }
    }

    /*
     * the remaining separated point pairs all must
     * have Hamming distance >= 3
     */
    clusters.numberOfGroups
  }
}

object BigClustering extends App {
  println(new BigClusteringAlgorithm("big_clusters_test_input.txt").run)
}