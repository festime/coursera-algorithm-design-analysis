package errorhandling

/*
 * Option 是一個最多只包含一個元素的 List
 *
 * [+A] 表示 covariant
 * 例如， Dog 是 Animal 的子類別
 * 那麼 Option[Dog] 也會是 Option[Animal] 的子類別
 * https://twitter.github.io/scala_school/type-basics.html#variance
 */
trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      /*
       * case Some(a: A) => Some(f(a))
       *
       * 多了 : A 會有警告
       * abstract type pattern A is unchecked since
       * it is eliminated by erasure
       *
       * 感覺是說這個 pattern match 不會去檢查 a 的型態
       * 畢竟如果都已經是 Option[Int], Option[Double] 之類的實例了
       * 那 A 明顯就是 Int, Double, ... 或是對應的型態了
       *
       * 下面其他 method 的實作用的 pattern match 也是一樣
       */
      case Some(a) => Some(f(a))
    }
  }

  /*
   * def flatMap[B](f: A => Option[B]): Option[B] =
   *   map(f) getOrElse None
   */
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
  }

  /*
   * >: 表示 B 要是 A 的父類別
   *
   * default: => B 表示 default 是 call by name
   * 不會在呼叫此函數時立即求值
   * 而是在函數內部若有執行到提及 default 的運算式
   * 才會求值
   */
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
   // this map ((a) => Some(a)) getOrElse ob
    this match {
      case None => ob
      case Some(a) => this
    }
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
//    this match {
//      case None => None
//      case Some(a) => if(f(a)) this else None
//    }
  }
}

case object None extends Option[Nothing]
case class Some[A](a: A) extends Option[A]

object Option {
  def apply[A](a: A): Option[A] = Some(a)

  /*
   * 正確性有疑問，暫時先跳過這個問題...
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = xs.sum / xs.length

    Some((xs.map(x => math.pow(x - mean, 2))).sum / xs.length)
  }



  /*
   * 這部分是要探討一個問題
   * Option 讓我們能用回傳值表達正確執行的結果、失敗、或錯誤
   * 但是否會造成程式碼遍佈需要檢查 Option 是 Some() 或 None 的情況？
   * 一些外部或第三方的方法或函數，在不是接受 Option 時要怎麼應對？
   * 因為這些函數我們通常無法修改
   * 或是修改它們的參數會連動到其他地方，因此不可行
   */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age + numberOfSpeedingTickets

  def parseInsuranceRateQuote(
          age: String,
          numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    /*
     * optAge 和 optTickets 都是 Option[Int]
     * 但是 insuranceRateQuote 是接收兩個 Int
     * 把 optAge 和 optTickets 裡的值取出再丟進去？如果它們是 None 怎辦？
     *
     * 書本提出的解法是
     * 實作一個中間人（或叫代理人？）輔助函數
     * 並 curry 化
     *
     * 第一對參數接受兩個 Option
     * 第二對參數接受一個一般函數，例如上述的 insuranceRateQuote ，它接收兩個 Int
     * 就可以在這個中間人函數（或代理人函數）裡面執行運算
     * 而那個一般函數、不接受 Option 的函數
     * 就可以對使用它的人是否多做了額外的事情、包裝
     * 一無所知也無所謂
     */
    // insuranceRateQuote(optAge, optTickets)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }

  /*
   * 開頭字母大寫，是為了跟語言的內建關鍵字 try 作區別
   *
   * 另一個重點是參數用 call by name
   * 這樣代表參數的 expression 就不會在呼叫此函數時立即被取值、爆 error
   * 而是到 Try 內部執行，提及參數時才取值
   *
   * 能正常取值的話就把這個值包進 Some 回傳
   * 否則就捕捉 error 然後回傳 None
   */
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None } // 這有丟棄錯誤訊息的問題，有待改進

  /*
   * 書本註解提到說
   * 這是一個不適合用 oo 風格定義函數的例子
   * 這不應該是 List 的一個方法，因為 List 不應對 Option 有任何了解
   * 也不能是 Option 的方法
   * 所以它被放在了 Option 的 companion object 裡
   * 不太理解為什麼這樣講？？？
   *
   * list 裡有一個是 None 就回傳 None
   * 否則就把 list 裡面所有 Option 都解包裝
   * 放進一個 List 再放進一個 Option 回傳
   *
   * 對 Option 的方法的特性實在不夠熟，尤其是 flatMap
   * 對 None 呼叫 flatMap 不會執行函數
   * 而是直接回傳 None
   * 這樣就能即使在 a 的中途遇到 None
   * 提前終止後續運算，直接回傳 None
   *
   * 想破頭都想不到要怎麼做...
   * 原來用 flatMap 的上述特性
   * 就可以提前終止
   *
   * def flatMap[B](f: A => Option[B]): Option[B]
   * def map[B](f: A => B): Option[B] =
   *
   * hh: A
   * sequence(t): Option[List[A]]
   * (hh :: _): List[A]
   *
   * where type of `_` is List[A] because sequence(t) is Option[List[A]]
   *
   * sequence(t) map (hh :: _) // Option[List[A]]
   * hh => sequence(t) map (hh :: _) // A => Option[List[A]]
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }
}

object Main extends App {
  println(Option.variance(Seq()))
  println(Option.variance(Seq(0, 1, 2, 3, 4)))
  println(Option.sequence(List(Option(1), Option(2), Option(3))))
  println(Option.sequence(List(Option(1), Option(2), None, Option(3))))
}