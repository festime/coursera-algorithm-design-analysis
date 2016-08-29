package schedule

import org.scalatest.FunSuite

class ScheduleSuite extends FunSuite {
  test("job 5#2 is larger than job 4#3") {
    assert(new Job(5, 2) > new Job(4, 3))
  }

  test("job 5#3 is larger than job 4#2") {
    assert(new Job(5, 3) > new Job(4, 2))
  }

  test("job 5#3 is larger than or equal to job 5#3") {
    assert(new Job(5, 3) >= new Job(5, 3))
  }

  test("sort a list of job") {
    val j1 = new Job(5, 3)
    val j2 = new Job(4, 2)
    val j3 = new Job(4, 3)
    val j4 = new Job(5, 2)
    val a = Array(j1, j2, j3, j4)

    assert(a.sorted(Ordering[Job].reverse) === Array(j4, j1, j2, j3))
  }

  test("correctness on test_input with difference as scores") {
    assert(new Schedule("test_input.txt").weightedSumOfCompletionTime === 69119377652L)
  }

  test("correctness on test_input with ratio as scores") {
    assert(new Schedule("test_input.txt", JobScore.ratioScore).weightedSumOfCompletionTime === 67311454237L)
  }
}