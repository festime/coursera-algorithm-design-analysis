package unionfind

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap

/*
 * More generic Union-Find data structure which
 * can support generic type, not just integers.
 */
class Union[T](input: Array[T]) {
  private val rank = HashMap[T, Int]()
  private val groupNumber = HashMap[T, Int]()

  for ((e, i) <- input.zipWithIndex) {
    rank(e) = 0
    groupNumber(e) = i
  }

  /*
   * With lazy union and union by rank
   *
   * 1 Union operation is composed of
   * 2 Find operations and 1 O(1) leader-update
   *
   * 1 Find operation is O(log n) running time,
   * where n is the number of elements in Union-Find.
   *
   * Thus, the running time of Union operation is O(log n).
   */
  def union(x: T, y: T): Unit = {
    val leaderOfX: T = find(x).get
    val leaderOfY: T = find(y).get

    if (rank(leaderOfX) < rank(leaderOfY)) {
      groupNumber(leaderOfX) = privateFind(leaderOfY)

    } else if (rank(leaderOfY) < rank(leaderOfX)) {
      groupNumber(leaderOfY) = privateFind(leaderOfX)

    } else {
      groupNumber(leaderOfY) = privateFind(leaderOfX)
      rank(leaderOfX) += 1
    }
  }

  /*
   * Because the maximum rank in a Union-Find
   * with lazy union and union by rank is less
   * than or equal to log_2(n), where n is the
   * number of elements in Union-Find.
   *
   * Thus, the running time of Find operation is O(log n).
   *
   * BTW, this implementation also uses path compression,
   * additional O(1) work in every layer of stack,
   * the running time is still O(log n).
   */
  def find(x: T): Option[T] = {
    if (groupNumber.getOrElse(x, -1) == -1) return None

    privateFind(x) match {
      case leaderIndex: Int if (leaderIndex >= 0 && leaderIndex < input.length) =>
        Some(input(leaderIndex))
    }
  }

  /*
   * O(log n) running time
   * The reason is similar to Find operation.
   */
  def sameGroup(x: T, y: T): Boolean =
    privateFind(x) == privateFind(y)

  /*
   * 實作 BigClustering 時問題出在這裡
   * 沒有對所有元素做過一次 path compression
   * 有些元素的 groupNumber 實際上還不是一個 group 的 root
   * 造成 group 數目多算
   *
   * O(n log n) running time???
   */
  def numberOfGroups: Int = {
    for (x <- input)
      privateFind(x)
    groupNumber.values.toSet.size
  }

  private def privateFind(x: T): Int = {
    if (input(groupNumber(x)) == x) {
      groupNumber(x)

    } else {
      val result = privateFind(input(groupNumber(x)))

      groupNumber(x) = result
      result
    }
  }
}
