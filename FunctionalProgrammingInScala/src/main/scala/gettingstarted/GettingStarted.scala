package gettingstarted

object GettingStarted extends App {
  /*
   * 一開始想不到要怎麼寫 tail recursive 的版本
   * 就先把直白的遞迴版本先實作出來
   * 再觀察之間的關係
   *
   * 想法就是特例情況 n = 0, 1 的時候直接回傳對應值
   * n >= 2 的時候才去呼叫輔助函數運算
   */
  def fib(n: Int): Int = {
    require(n >= 0)

    @annotation.tailrec
    def helper(m: Int, a: Int, b: Int): Int = {
      if (m < n)
        helper(m + 1, a + b, a)
      else
        a + b
    }

    if (n == 0)
      0
    else if (n == 1)
      1
    else
      helper(2, 1, 0)
//      fib(n - 1) + fib(n - 2)
  }

  /*
   * 和上一個實作的想法有點類似
   * 我先根據 input 的陣列的 length 做判斷
   * 特殊情況， length = 0 or 1 的時候直接回傳 true
   * length >= 2 才去呼叫輔助函數運算
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(index: Int): Boolean = {
      index match {
        case i if (i >= as.length) => return true
        case _ =>
      }

      ordered(as(index - 1), as(index)) match {
        case true => loop(index + 1)
        case false => false
      }
    }

    as.length match {
      case length if (length == 0 || length == 1) => true
      case _ => loop(1)
    }
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    // equally, (a: A) => ((b: B) => f(a, b))
    //                    |                 |
    // because => is right-associative
    // or
    // a => b => f(a, b)
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    // (a: A, b: B) => f(a)(b)
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    // (a: A) => f(g(a))
    a => f(g(a))





  for (i <- 0 to 10) println(fib(i))

  val compareFunction = (n: Int, m: Int) => n < m
  for (n <- 0 to 10) {
    val a = (0 to n).toArray
    println(isSorted(a, compareFunction))
  }

  println(isSorted(Array(0, 1, 2, 3, 2), compareFunction))
}