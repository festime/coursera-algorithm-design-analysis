package scc

import org.scalatest.FunSuite

class KosarajuAlgorithmSuite extends FunSuite {
  test("2 === 1 + 1") {
    assert(2 === 1 + 1)
  }

  test("correctness on small_input_1") {
    assert((new Graph("small_input_1.txt").sccCount) === "3,3,3,0,0")
  }

  test("correctness on small_input_2") {
    assert((new Graph("small_input_2.txt").sccCount) === "3,3,2,0,0")
  }

  test("correctness on small_input_3") {
    assert((new Graph("small_input_3.txt").sccCount) === "3,3,1,1,0")
  }

  test("correctness on small_input_4") {
    assert((new Graph("small_input_4.txt").sccCount) === "7,1,0,0,0")
  }

  test("correctness on small_input_5") {
    assert((new Graph("small_input_5.txt").sccCount) === "6,3,2,1,0")
  }

  test("correctness on test_input") {
    assert((new Graph("test_input.txt").sccCount) === "434821,968,459,313,211")
  }
}