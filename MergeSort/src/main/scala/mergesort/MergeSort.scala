package mergesort

object MergeSort {
  def sort(input: Array[Int]): Array[Int] = {
    if (input.length <= 1) {
      input

    } else {
      val mid = input.length / 2
      val frontHalf = sort(input.slice(0, mid))
      val backHalf = sort(input.slice(mid, input.length))

      merge(frontHalf, backHalf)
    }
  }

  private def merge(array1: Array[Int], array2: Array[Int]): Array[Int] = (array1, array2) match {
    case (Array(), array2) => array2
    case (array1, Array()) => array1
    case (Array(n1, _*), Array(n2, _*)) =>
      if (n1 < n2) n1 +: merge(array1.tail, array2)
      else n2 +: merge(array1, array2.tail)
  }
}

object Main extends App {
  println(MergeSort.sort(Array(2, 1, 5, 4, 3)).mkString(", "))
}