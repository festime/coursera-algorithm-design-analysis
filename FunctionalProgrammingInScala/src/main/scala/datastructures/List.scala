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
   */
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  /*
   * _ * _ 的版本覺得太容易混淆了
   * 還是明確的寫 (x, y) => x + y 比較合我胃口
   * 之所以 x, y 不用指定型態是因為 Scala 編譯器幫忙推導了
   */
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)((x, y) => x * y)
    // foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => 1 + 1)
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
    case Cons(h, t) => h + List.sum(t)
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
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
  println(List.length(List(0, 1, 2, 3, 4)))
}