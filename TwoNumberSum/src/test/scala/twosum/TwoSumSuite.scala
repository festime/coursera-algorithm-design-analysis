package twosum

import org.scalatest.FunSuite

class TwoSumSuite extends FunSuite {
  test("correctness on test input") {
    assert(new TwoSum("test_input.txt").countDistinctNumberPair() === 427)
  }
}