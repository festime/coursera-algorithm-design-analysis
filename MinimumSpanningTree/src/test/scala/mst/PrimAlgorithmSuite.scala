package mst

import org.scalatest.FunSuite

class PrimAlgorithmSuite extends FunSuite {
  test("correctness on test_input") {
    assert(new PrimAlgorithm("test_input.txt").run === -3612829)
  }
}