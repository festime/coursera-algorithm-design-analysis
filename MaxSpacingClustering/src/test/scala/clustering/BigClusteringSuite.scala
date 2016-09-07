package clustering

import org.scalatest.FunSuite

class BigClusteringSuite extends FunSuite {
  test("correctness on big_clustering_test_input") {
    assert(new BigClusteringAlgorithm("big_clusters_test_input.txt").run === 6118)
  }
}