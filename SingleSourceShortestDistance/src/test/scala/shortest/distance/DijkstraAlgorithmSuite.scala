package shortest.distance

import org.scalatest.FunSuite

class DijkstraAlgorithmSuite extends FunSuite {
  test("correct shortest path distance from source: test_input_1.txt") {
    assert(
     (new DijkstraAlgorithm("small_test_input_1.txt"))
       .shortestDistances
       .tail
       .toList ===
         List(0, 1, 2, 3, 4, 4, 3, 2)
    )
  }

  test("correct shortest path distance from source: test_input_2.txt") {
    assert(
     (new DijkstraAlgorithm("small_test_input_2.txt"))
       .shortestDistances
       .tail
       .toList ===
         List(0, 1, 4, 5, 3, 4, 3, 2, 3, 6, 5)
    )
  }

  test("correct shortest path distance from source: test_input.txt") {
    val algorithm = new DijkstraAlgorithm("test_input.txt")
    val indices = Array(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)

    val output = indices.map {
      n => algorithm.shortestDistances(n)
    }.mkString(",")

    assert(output === "2599,2610,2947,2052,2367,2399,2029,2442,2505,3068")
  }
}