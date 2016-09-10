package datastructures

/*
 * sealed 關鍵字表示這個 trait `List` 的所有實作
 * 必須都在這個檔案內
 *
 * [+A] 表示 covariant
 * 例如， Dog 是 Animal 的子類別
 * 那麼 List[Dog] 也會是 List[Animal] 的子類別
 * https://twitter.github.io/scala_school/type-basics.html#variance
 */
sealed trait List[+A]

/*
 * Scala 會為 case object, case class 生成預設的 toString
 * 但是預設的 toString 是以遞迴方式實現
 * 如果 List 很長，可能會發生 StackOverflow error
 * 你可能會想自己實作 toString
 *
 * Nothing 是所有類別的子類別
 * 也就是說 List 使用 covariant 後
 * Nil 可以當成如 List[Int], List[Double] 等類別的具體實例
 */
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/*
 * companion object
 * 跟定義的 class 或 trait 同名的一個 object
 * 通常會在 companion object 裡放一些方便使用的輔助方法
 * 目前裡面放的都是跟 list 相關的方法
 *
 * 其實把它叫做 Foo, Bar, whatever 都可
 * 只是因為我們的 trait 就叫 List
 * 所以也叫 List 最清晰直接
 */
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /*
   * 參數裡的 A* 表示 as 可以是 0 個或多個類別為 A 的參數
   *
   * 一個類別，在 companion object 定義 variadic function `apply`
   * 以便產生這個類別的實例，是一種慣例
   * 透過這樣定義
   * 我們就可以用像 List(0, 1, 2), List("a", "b") 這種方式
   * 來產生 List 實例，不管多少值，只要以逗號隔開就好
   *
   * 在 else 分枝裡呼叫 apply 代的 argument 出現的奇怪符號 _*
   * 叫做 type ascription
   * 詳細原理一時不是很明白
   * http://docs.scala-lang.org/style/types.html#ascription
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /*
   * O(1) running time
   *
   * 如何用其他方式處理或表達例外，下章節會討論這部分
   */
  def head[A](as: List[A]): A = as match {
    case Nil => throw new Error("head of Nil")
    case Cons(h, t) => h
  }

  /*
   * O(1) running time
   *
   * Nil 的情況要怎麼處理，值得好好思考，書的下章節會討論到這部分
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  /*
   * O(1) running time
   */
  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(a, t)
  }

  /*
   * O(n) running time, where `n` is the argument of drop
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0)

    n match {
      case i if (i > 0) => drop(List.tail(l), n - 1)
      case 0 => l
    }
  }

  /*
   * running time depends on `l` 開頭連續有多少元素
   * 會滿足 f 這個條件
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if (f(h)) dropWhile(t, f)
      else l
    }
  }

  /*
   * running time depends on the size of `a1`
   */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /*
   * O(n) running time, where n is the size of `l`
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /*
   * curry 化的 dropWhile 版本
   * 邏輯是和原版本一樣
   * 這麼改的好處之一是幫助型別推導
   * 就可以這麼使用：
   * curriedDropWhile(List(0, 1, 2, 3, 4))(x => x < 3)
   * 因為在第一對括號裡的參數，就確定了 A 是 Int
   * 所以第二對括號裡的匿名函數
   * 就可以在這個匿名函數的參數部分省略指定型態
   */
  def curriedDropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => curriedDropWhile(t)(f)
      case _ => as
    }

  /*
   * z 等價於操作的起始值
   * 以下面 sum2 的例子來說
   * 要總和一個 list 內所有數字，所需起始值就是 0
   *
   * product2 要一個 list 內所有數字的乘積
   * 所需起始值就是 1.0
   *
   * 需要特別注意的是
   * 這個遞迴版本的 foldRight 會一直增加 stack 層數
   * 直到 List 末端為止
   * 所以有可能會發生 stack overflow error
   */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /*
   * sum2(List(1, 2, 3))
   * 1 + (2 + (3 + 0))
   * 1 + (2 + 3)
   * 1 + 5
   * 6
   *
   * 圖像化，更容易理解
   *    +
   *  /   \
   * 1      +
   *      /   \
   *     2      +
   *          /   \
   *         3     0 (給 foldRight curry 化的第二個參數 `某個操作函數` 吃的預設值)
   */
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  /*
   * _ * _ 的版本覺得太容易混淆了
   * 還是明確的寫 (x, y) => x + y 比較明確、合我胃口
   * 之所以 x, y 不用指定型態是因為 Scala 編譯器幫忙推導了
   *
   * product2(List(1.0, 2.0, 3.0))
   *    *
   *  /   \
   * 1      *
   *      /   \
   *     2      *
   *          /   \
   *         3     1 (給 foldRight curry 化的第二個參數 `某個操作函數` 吃的預設值)
   * 省略小數點，方便直接複製上面的圖小改就好
   *
   * 習題有一題問說有沒有可能用 foldRight 實作 product 時
   * 遇到 0 時就直接回傳 0 不再遞迴下去，省去多餘的計算？
   * 目前的 foldRight 版本不行！
   * 因為它一定要先遞迴到 List 最後一個元素
   * 再倒車一個一個操作
   */
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => x * y)
    // foldRight(ns, 1.0)(_ * _)

  /*
   *    +
   *  /   \
   * 1      +
   *      /   \
   *     1      +
   *          /   \
   *         1     0 (給 foldRight curry 化的第二個參數 `某個操作函數` 吃的預設值)
   * 依照這個圖像化的方式思考
   * 怎麼實作 length 就一目了然
   * operation 不再需要第一個參數，一律回傳 1 即可，因為一個元素長度就算 1 而已
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, accumulation) => 1 + accumulation)
  }

  /*
   * e.g.
   *       +
   *      / \
   *     +   3
   *    / \
   *   +   2
   *  / \
   * 0   1
   *
   * 變成 operation 先對 List 開頭的元素操作
   * 然後依序往後一個一個操作
   * 很容易寫尾端遞迴的版本
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def helper(ls: List[A], accumulation: B): B = {
      ls match {
        case Nil => accumulation
        case Cons(h, t) => helper(t, f(accumulation, h))
      }
    }

    helper(as, z)
  }

  def sum3(ns: List[Int]): Int = {
    foldLeft(ns, 0)((x: Int, y: Int) => x + y)
  }

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)((x: Double, y: Double) => x * y)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((accumulation, _) => accumulation + 1)

  /*
   * reverse(List(1, 2, 3))
   *
   *           Cons
   *          /    \
   *        Cons    3
   *       /    \
   *     Cons    2
   *    /    \
   * Nil      1
   *
   * Nil 是 operation 的運算元之一的起始值
   * 第一次運算實際上是 Cons(1, Nil)
   *
   * 但為了準確表達 foldLeft 的意義
   * 所以還是用如此順序做圖像化顯示
   */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((y: List[A], x: A) => Cons(x, y))
  }

  /*
   *    Cons
   *   /    \
   * as(0)   Cons
   *        /    \
   *      as(1)   Cons
   *             /    \
   *           as(2)   Cons
   *                  /    \
   *                as(3)    ...
   *                            \
   *                             Cons
   *                            /    \
   *                          as(n)   bs
   */
  def append2[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs)((x: A, xs: List[A]) => Cons(x, xs))
  }

  /*
   *    append
   *   /      \
   * a(0)      append
   *          /      \
   *        a(1)      append
   *                 /      \
   *               a(2)      Nil
   *
   */
  def combine[A](a: List[List[A]]): List[A] = {
    foldRight(a, Nil: List[A])((as: List[A], xs: List[A]) => append2(as, xs))
  }

  def addOneToEachElement(ns: List[Int]): List[Int] = ns match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, addOneToEachElement(t))
  }

  def doubleToString(ns: List[Double]): List[String] = ns match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) {
        Cons(h, filter(t)(f))
      } else {
        filter(t)(f)
      }
  }

  /*
   *         append
   *        /      \
   *      append    f(as(2))
   *     /      \
   *   append    f(as(1))
   *  /      \
   * Nil      f(as(0))
   *
   * 最下面的 Nil 和 f(as(0))
   * 其實 f(as(0)) 是 append 的第一個參數， Nil 是第二個參數
   *
   * e.g.
   * flatMap(List(1, 2, 3))(i => List(i, i)) => List(1, 1, 2, 2, 3, 3)
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, Nil: List[B])((bs: List[B], a: A) => append(bs, f(a)))
  }

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((a: A) => if (f(a)) List(a) else Nil: List[A])
  }

  /*
   *    Cons
   *   /    \
   * x1+y1   Cons
   *        /    \
   *      x2+y2   Cons
   *             /    \
   *           x3+y3   Nil
   *
   * 為什麼不能用 foldRight 或 foldLeft ？
   * 它們兩個接受的第一個參數都是一個 List
   */
  def correspondingSum(xs: List[Int], ys: List[Int]): List[Int] = {
    /*
     * 奇技淫巧...靈光一閃想到的，想不到真的可以這樣用
     */
    (xs, ys) match {
      case (Nil, Nil) => Nil
      case (Cons(h1, t2), Nil) => xs
      case (Nil, Cons(h2, t2)) => ys
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Cons(h1 + h2, correspondingSum(t1, t2))
    }
  }

  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = {
    (xs, ys) match {
      case (Nil, Nil) => Nil
      case (Cons(h1, t2), Nil) => xs
      case (Nil, Cons(h2, t2)) => ys
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    as match {
      case Nil =>
        if (length(sub) == 0)
          true
        else
          false
      case Cons(h, t) =>
        if (h == head(sub))
          hasSubsequence(t, tail(sub))
        else
          hasSubsequence(t, sub)
    }
  }

  /*
   *        <
   *       / \
   *      <   3
   *     / \
   *    <   2
   *   / \
   * Max  1
   */
  def min(ns: List[Int]): Int = {
    foldLeft(ns, Int.MaxValue)((x: Int, y: Int) => if (x < y) x else y)
  }

  /*
   * 用 foldLeft 實作 foldRight
   * 可以避免初始版本 foldRight 可能發生 stack overflow error 的問題
   *
   * 因為 reverse 用 foldLeft 實作
   * 而 foldLeft 是用尾端遞迴實作
   * 所以不會有 stack overflow error 的問題
   *
   * 一個小小缺點是
   * 因為 foldRight 和 foldLeft 接受的 operation 函數
   * 的簽名參數順序不一樣，需要多寫一點點程式碼轉一下
   */
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
  }
}

object Main extends App {
//  println(List.sum(List(1, 2)))
//  println(List.sum(List()))
//  println(List.product(List(0, 1)))
//  println(Cons(1, Nil))
//  println(List(0, 1, 2, 3))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(x)

  val l: List[Int] = Nil
//  println(l)

//  val xs = List(Seq(0, 1))
//  println(xs)

//  println(List.tail(List(0, 1, 2, 3)))

  val ys = List(0, 1, 2)
//  println(List.setHead(ys, -1))
//
//  println(List.drop(List(0, 1, 2, 3, 4, 5), 2))
//  println(List.dropWhile(List(0, 1, 2, 3, 4, 5), (n: Int) => n < 3))
//  println(List.append(List(0, 1, 2, 3), List(4, 5, 6)))
//  println(List.init(List(0, 1, 2, 3, 4)))
//  println(List.init(List(0)))

  println(List.foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))
  println("length = " + List.length(List(0, 1, 2, 3, 4)))
  println("sum3 of List(1, 2, 3, 4) = " + List.sum3(List(1, 2, 3, 4)))
  println("product3 of List(2, 3, 4, 5) = " + List.product3(List(2, 3, 4, 5)))
  println("length 3 of List(1, 2, 3, 4, 5, 6, 7) = " + List.length3(List(1, 2, 3, 4, 5, 6, 7)))
  println("reverse of List(0, 1, 2, 3)" + List.reverse(List(0, 1, 2, 3)))
  println("List(0, 1, 2, 3) append List(4, 5, 6) = " + List.append2(List(0, 1, 2, 3), List(4, 5, 6)))
  println(List.combine(List(List(0), List(1, 2), List(3, 4, 5), List(6, 7, 8, 9))))

  println(List.addOneToEachElement(List(0, 1, 2, 3, 4)))
  println(List.doubleToString(List(0.0, 1.0, 2.0, 3.0)))
  println(List.map(List(0, 1, 2, 3, 4))((n: Int) => n + 1))
  println(List.filter(List(0, 1, 2, 3, 4, 5))((n: Int) => n % 2 == 0))
  println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
  println(List.filterByFlatMap(List(0, 1, 2, 3, 4, 5))((n: Int) => n % 2 == 0))
  println(List.correspondingSum(List(1, 2, 3), List(4, 5, 6)))
  println(List.correspondingSum(List(1, 2, 3), List(4, 5, 6, 7, 8)))
  println(List.zipWith(List(1, 2, 3), List(4, 5, 6, 7, 8))((x, y) => x + y))

  println(List.hasSubsequence(List(0, 1, 2, 3, 4), List(2, 4)))
  println(List.hasSubsequence(List(0, 1, 2, 3, 4), List(2, 6)))
  println(List.min(List(1, 2, 0, 3, 4, 5)))
}