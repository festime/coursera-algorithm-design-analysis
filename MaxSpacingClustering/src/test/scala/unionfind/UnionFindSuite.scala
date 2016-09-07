package unionfind

import org.scalatest.FunSuite

class UnionFindSuite extends FunSuite {
  val uf = new Union((0 to 10).toArray)

  test("union and sameGroup case 1") {
    uf.union(0, 1)

    assert(uf.sameGroup(0, 1))
  }

  test("union and sameGroup case 2") {
    uf.union(3, 4)
    uf.union(4, 5)

    assert(uf.sameGroup(3, 5))
  }

  test("union and sameGroup case 3") {
    uf.union(6, 7)
    uf.union(7, 6)

    assert(uf.sameGroup(6, 7))
  }

  test("sameGroup case 1") {
    assert(uf.sameGroup(9, 10) === false)
  }
}