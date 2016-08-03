package graphminimumcut

import org.scalatest.FunSuite

class GraphMinimumCutSuite extends FunSuite {
//  test("2 === 1 + 1") {
//    assert(2 === 1 + 1)
//  }
  test("output of this small input should be 2") {
    val graph = new GraphMinimumCut("small_input.txt")
    assert(graph.minCut() === 2)
  }
  
  test("output of this permutated small input should be 2") {
    val graph = new GraphMinimumCut("permutated_small_input.txt")
    assert(graph.minCut() === 2)
  }

  test("output of another small input should be 1") {
    val graph = new GraphMinimumCut("another_small_input.txt")
    assert(graph.minCut() === 1)
  }

  test("output of permutated another small input should be 1") {
    val graph = new GraphMinimumCut("permutated_another_small_input.txt")
    assert(graph.minCut() === 1)
  }

  test("output of this large input should be 3") {
    val graph = new GraphMinimumCut("large_input.txt")
    assert(graph.minCut() === 3)
  }

  test("output of test input should be 17") {
    val graph = new GraphMinimumCut("test_input.txt")
    assert(graph.minCut() === 17)
  }
}