package clustering

import org.scalatest.FunSuite

class ClusteringSuite extends FunSuite {
  test("UnionFind find") {
    val unionFind = new UnionFind(10)

    for (i <- 0 to 10) assert(unionFind.find(i) === i)
    assert(unionFind.numberOfGroups === 10)
  }

  test("UnionFind union") {
    val unionFind = new UnionFind(10)

    unionFind.union(1, 2)
    unionFind.union(1, 3)

    assert(unionFind.find(1) === unionFind.find(2))
    assert(unionFind.find(1) === unionFind.find(3))
    assert(unionFind.numberOfGroups === 8)
  }

  test("correctness on test_input") {
    assert(new MaxSpacingClusteringAlgorithm("test_input.txt").run(4) === 106)
  }
}