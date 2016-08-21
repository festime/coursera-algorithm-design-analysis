package median

import org.scalatest.FunSuite

class MedianMaintenanceSuite extends FunSuite {
  test("correctness on test input") {
    assert(
      new MedianMaintenance("median_test_input.txt").mediansSumMod === 1213)
  }
}