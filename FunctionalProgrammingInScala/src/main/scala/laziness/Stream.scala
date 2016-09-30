package laziness

/*
 * Stream[+A] 的 +A 表示
 * 如果 Animal 是 Dog 的父類別
 * 那麼 Stream[Animal] 也會是 Stream[Dog] 的父類別
 */
sealed trait Stream[+A] {
  /*
   * 因為不是尾端遞迴，有 stack overflow 的疑慮
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: (t().toList)
  }

  /*
   * 因為遞迴過程的 h() :: acc
   * 是從第一個元素開始依序到最後一個元素
   * 所以整個流程執行完畢時
   * 會是原本 Stream 的最後一個元素在最前面
   * 原本 Stream 的第一個元素反而是在最後面
   * 順序剛好整個顛倒，所以為了回傳原本順序
   * 需要最後把整個 list 再 reverse
   *
   * 如果是每輪都 append 元素到 list 最後面
   * 雖然結果也是正確
   * 但會造成需要遍歷 list 到最末端
   * 需要的執行時間就會變成
   * 1 + 2 + 3 + ... + (n - 1) + n = O(n ^ 2)
   * 可以嘗試比較兩版本的執行時間，明顯有差
   */
  def toListTailRecursive: List[A] = {
    @annotation.tailrec
    def helper(ss: Stream[A], acc: List[A]): List[A] = ss match {
      case Empty => acc
      case Cons(h, t) => helper(t(), h() :: acc)
    }

    helper(this, List[A]()).reverse
  }

  /*
   * 一樣是 tail recursive
   * 但是用 mutable data structure ListBuffer 輔助
   * 查過文件，寫 ListBuffer 做 append 和 prepend 都是 O(1)
   * 且它轉成 List 也是 O(1)
   * 對解決這個問題的流程相當匹配和有用
   *
   * 就時間複雜度來看，雖然和 toListTailRecursive 一樣都是 O(n)
   * 但是最後一步的差別
   * 一個是 O(1) 的 toList
   * 一個是 O(n) 的 reverse
   * 會差多少？
   */
  def toListViaListBuffer: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /*
   * 其實 n 可以不用寫進 match 和 case 裡
   * 可以直接在 case 後面加條件式判斷
   * 看起來稍微簡潔清楚一點
   */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => Stream.cons(h(), Empty)
      case _ => Empty
    }
//    這個是我的實作
//    (this, n) match {
//      case (_, n) if (n <= 0) => Empty
//      case (Empty, _) => Empty
//      case (Cons(h, t), n) => Cons(h, () => t().take(n - 1))
//    }
  }

  /*
   * 一定要對 public tail recursive 標記 final 關鍵字的原因
   * 這篇有討論：
   * http://stackoverflow.com/a/4785606/3295962
   *
   * 大意是，假設父類別定義了一個自身遞迴呼叫的方法 f
   * 子類別 override 了父類別的 f
   * 並在裡面有透過 super 去呼叫父類別的 f
   * 這時若產生一個子類別的實例，並對其呼叫 f 的話
   * 會發生
   * Child.f call Parent.f,
   * then Parent.f call Child.f,
   * then Child.f call Parent.f, etc
   * 換句話說，一個呼叫自身的遞迴方法，若沒標記 final ，例如上述的 Parent.f
   * 未必會真的呼叫自己！
   */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    /*
     * 和上面 take 的實作一樣，其實 n 可以不用寫在 match 裡
     * 另外一個問題是 drop 和 take 有根本上的不同
     * take 是取 stream 前 n 個元素的值
     * 可以用 Stream.cons(h(), t().take(n-1)) 來延後對 tail 的求值
     * 所以每次對當前 tail 求值，並不會一直遞迴下去
     *
     * 但 drop 不一樣，一定要對 stream 前 n 個元素求值過後
     * 最後回傳到達的節點（第 n + 1 個節點，如果有的話）
     * 如果 stream 和 n 都夠大，可能會 stack overflow
     * 可以用尾端遞迴實作來避免
     */

//    原本實作，未使用尾端遞迴
//    (this, n) match {
//      case (_, n) if (n <= 0) => this
//      case (Empty, _) => Empty
//      case (Cons(_, t), n) => t().drop(n - 1)
//    }
  }

  /*
   * 因為使用了 Stream.cons 這個額外自定義的 constructor
   * 兩個 arguments 都是 call by name
   * 它內部使用了兩個 lazy val 參考傳給它的兩個 arguments
   * 並用這兩個 lazy-evaluation variable 來構造 stream
   * Cons(() => h, () => t)
   * Cons 的兩個 arguments 也是 call by name
   * 所以 Stream.cons 兩個 arguments 在整個過程都不會被立即求值
   *
   * 原 stream 的 takeWhile 結果（另一個 stream ）
   * 除非對這個結果的 tail 求值
   * 否則 t().takeWhile(p) 並不會立即被執行
   * 換句話說，就是延後了求值，必要的時候才求值
   *
   * 但是這個實作在原 stream 夠大、從開頭滿足條件的連續元素夠多的話
   * 有 stack overflow 疑慮
   */
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) == true => Stream.cons(h(), t().takeWhile(p))
      case Cons(h, t) if p(h()) == false => Empty
      case _ => Empty
    }
  }

  /*
   * 無論是這個最直觀版本的 exists
   * 或是下面透過 foldRight 實作的 exists
   * 在 stream 夠大
   * 且在超過一定數目仍然找不到符合條件的元素時
   * 都有可能 stack overflow
   */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  /*
   *    op
   *   /  \
   * s(0)  op
   *      /  \
   *    s(1)  op
   *         /  \
   *       s(2)  ...
   *                \
   *                 op
   *                /  \
   *              s(n)  z
   *
   * 這個實作，結構乍看與 List 的 foldRight 非常類似
   * 但由於 B 型態的 argument 是 call by name
   * foldRight 第一對括號的參數是 call by name
   * op 函數的型態 B 的 argument 也是 call by name
   * 而有根本上的不同
   *
   * 因為 op 函數吃的第二個參數是 call by name
   * 呼叫當下不會立即對 t().foldRight(z)(f) 求值
   * 換句話說，不會對當前 stream 的 tail 馬上遞迴下去
   * 而是 op 函數執行過程中若有提及第二個參數
   * 才會對其求值
   * 如果 op 函數執行過程
   * 只要對第一個參數「當前 stream 的 head 」求值後
   * 就能決定提前終止或回傳值
   * foldRight 就不會繼續遞迴下去
   *
   * 之前 List 的 foldRight 之所以無法提前終止
   * 因為 foldRight 一直對當前 list 的 tail 遞迴呼叫 foldRight
   * 直到 list 最後一個元素
   * 才開始與指定的初始值做運算
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /*
   *         op
   *        /  \
   * s(0) => F  op
   *           /  \
   *    s(1) => F  op
   *              /  \
   *       s(2) => T  X （提前終止了，不繼續下去）
   */
  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) if (t() == Empty) => p(h())
      case Cons(h, t) => p(h()) && t().forAll(p)
      case Empty => true
    }
  }

  /*
   * 雖然遇到不滿足的會停止遞迴下去沒錯
   * 但是如果 stream 夠大且一直沒有不滿足條件的元素的話
   * 還是有 stack overflow 的疑慮
   */
  def forAllViaFoldRight(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  // foldRight[B](z: => B)(f: (A, => B) => B): B =
  /*
   * takeWhile 要求做的事情、本身的邏輯
   * 使得執行過程一定得一直依序對 Stream 的元素求值、檢查是否滿足條件
   * 直到遇到 stream 當中第一個不滿足條件的元素或最末端為止
   */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((h, tail) =>
      if (p(h)) Stream.cons(h, tail)
      else      Empty: Stream[A])

//    當條件不滿足時回傳 tail 是錯的
//    原因是還沒遞迴到最後一個元素時的 tail
//    是 t().foldRight(z)(f)
//    求值的結果未必會是 Empty: Stream[A]

//    foldRight(Empty: Stream[A])((h, tail) => if (p(h)) Stream.cons(h, tail) else tail)
  }

  /*
   * 只要 stream 有元素、 stream 不是 Empty
   * 就打包第一個元素進 Some 回傳
   * 反之則回傳 None
   * 說真的用 foldRight 不太直觀
   * 也沒什麼效能上的優勢
   * 純粹是考你對 stream 的 foldRight 邏輯有沒有掌握而已
   */
  def headOptionViaFoldRight: Option[A] = {
    foldRight(None: Option[A])((h, _) =>
      if (h != Empty) Some(h)
      else            None: Option[A])

//    和 takeWhileViaFoldRight 一樣有類似的問題
//    none 未必是 None: Option[A]

//    foldRight(None: Option[A])((h, none) => if (h != Empty) Some(h) else none)
  }

  def mapViaFoldRight[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((h, tail) => Stream.cons(f(h), tail))
  }

  def filterViaFoldRight(f: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((h, tail) => if (f(h)) Stream.cons(h, tail) else tail)
  }

  /*
   * 這裡為什麼非得寫 B >: A （表示 B 是 A 的父類別）
   * 而且 argument 的型態是 Stream[B] ？
   */
  def append[B >: A](ss: => Stream[B]): Stream[B] = {
    foldRight(ss)((h, tail) => Stream.cons(h, tail))
//    這樣寫邏輯也是對的，只是稍微多了一點點程式碼
//    其實因為 foldRight 的特性，如果一直對 stream 的後續元素求值的話
//    就會一直往 stream 後面遞迴求值，到最後一個元素的時候
//    就會動用到傳給 foldRight 的初始值了，所以一樣寫 Stream.cons(h, tail) 即可
//    這時的 tail 就是要 append 的另一個 stream
//    foldRight(ss)((h, tail) => if (h != Empty) Stream.cons(h, tail) else tail)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((h, tail) => f(h) append tail)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }
  /*
   * 當 n > 0 且 stream 內還有元素時就繼續取
   * 當 stream 為空時，不繼續
   * 當 n <= 0 時，不繼續
   */
  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case (Empty, _) => None
      case _ => None
    }
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }
//  def zipWithViaUnfold(s2: Stream[A])(f: (A, A) => A): Stream[A] = {
  def zipWithViaUnfold[B >: A](s2: Stream[B])(f: (A, B) => B): Stream[B] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some((h1(), (t1(), Empty)))
      case (Empty, Cons(h2, t2)) => Some((h2(), (Empty, t2())))
      case _ => None
    }
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

  /*
   * e.g., Stream(1, 2, 3) startsWith Stream(1, 2) => true
   */
  def startsWith[A](s: Stream[A]): Boolean = {
    (this, s) match {
      case (Cons(h1, t1), Cons(h2, t2)) if (h1() == h2()) =>
        t1().startsWith(t2())
      case ((Cons(h1, t1), Empty)) => true
      case (Empty, Cons(h2, t2)) => false
      case (Empty, Empty) => true
    }
  }

  /*
   * e.g.
   * Stream(1, 2, 3).tails
   * => Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream())
   */
  def tails: Stream[Stream[A]] = {
    Stream.cons(
      this,
      unfold(this) {
        case Cons(h, t) => Some((t(), t()))
        case Empty => None
      }
    )
  }
}

/*
 * Nothing 是所有其他型別的子類別
 * Empty 定義成一個 case object 且 extends Stream[Nothing]
 * 再加上把 Stream 的 type parameter 用 +A 定義，是 covariant
 * 使得 Empty 是 Stream[A] 的實例，無論 A 是什麼型態
 *
 * Cons constructor 接受兩個無參數函數
 * 使用它時可以臨時定義 function literals 代入
 * 或是代入用 def 關鍵字定義符合簽名檔和回傳值型態的 function
 *
 * e.g.
 * def test(f: () => Unit) = f()
 * def f() = println("hello")
 *
 * test(() => println("hello"))
 * test(f)
 *
 * 一個 expression 的未求值形式叫做 thunk
 * 想要對一個 thunk 求值，要像上述 test 的定義裡面那樣
 * f()
 * 如呼叫函數那樣
 */
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

/*
 * 在 Stream companion object 定義 apply 吃 variadic arguments
 * 就可以用像 Stream(0, 1, 2) 這種方式方便初始化一個 Stream
 */
object Stream {
  /*
   * 這個構造器目的是為了惰性求值 Stream 的 head 和 tail
   * 設 lazy val head 和 tail 儲存傳遞給它的對應參數 hd, tl
   * 並用於初始化一個 Cons 實例
   * 回傳 Cons(() => head, () => tail)
   * hd 和 tl 整個過程其實完全沒有被求值
   *
   * e.g.
   * case class C(f: () => Unit)
   * def test(f: () => Unit) = C(f)
   *
   * test(() => println("hello"))
   * => C = C(<function0>)
   *
   * test 的 argument 是 call by name
   * 上述測試發現
   * 沒有印出 hello 字串，換句話說，傳給 test 的 argument 並沒被求值
   *
   * 使用 lazy-evaluation variables 參考 hd 和 tl
   * 再使用在 Cons 這個 case class constructor
   * 其兩個 arguments 也是 call by name
   * 使得 hd, tl 在 cons 整個流程之中都未被求值
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
//    This is more efficient than `cons(a, constant(a))` since it's just
//    one object referencing itself.
//
//    lazy val tail: Stream[A] = Cons(() => a, () => tail)
//    tail
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  /*
   * 邏輯是對，不過抽象化做得不太好
   * 其實傳遞 2 個整數，把第一個參數當作當前要建立的 stream 的 head 即可
   * 第一輪是 s_1, s_2 = (0, 1)
   * 第二輪是 s_2, s_3
   * 第三輪是 s_3, s_4 依此類推
   */
  def fibs(): Stream[Int] = {
    def helper(x: Int, y: Int): Stream[Int] = {
      Stream.cons(x + y, helper(y, x + y))
    }

    Stream.cons(0, Stream.cons(1, helper(0, 1)))
//    def go(f0: Int, f1: Int): Stream[Int] =
//      cons(f0, go(f1, f0 + f1))
//    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Empty
    }
  }

  def onesViaUnfold(): Stream[Int] = {
//    could also of course be implemented as constant(1)
    unfold(1)(n => Some((1, 1)))
  }
  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(a => Some((a, a)))
  }
  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(n => Some((n, n + 1)))
  }
  /*
   * 自己實作的版本抽象化做得不好，寫出來的程式碼才會如此複雜
   */
  def fibsViaUnfold(): Stream[Int] = {
//    unfold 是 curry 化的，傳遞第一對參數呼叫執行後
//    回傳另一個接受一個參數的函數
//    當一個函數只接受一個參數時，可在呼叫時用 {} 替代 ()
//    下面這寫法算是 argument pattern match?
//    就可以 decompose 一個 pair argument?
    unfold((0, 1)) {
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    }
//    Stream.cons(
//      0,
//      Stream.cons(
//        1,
//        unfold((0, 1))(intPair => Some(intPair._1 + intPair._2, (intPair._2, intPair._1 + intPair._2)))))
  }
}

object Main extends App {
  println(Stream(0, 1, 2, 3, 4).toList)
  println(Stream(0, 1, 2, 3, 4).toListTailRecursive)
  println(Stream(0, 1, 2, 3, 4).toListViaListBuffer)
  println(Empty.toList)
  // StackOverflowError
  // Stream((0 to 10000000).toList: _*).toList
  //
  // OutOfMemoryError
  // Stream((0 to 10000000).toList: _*).toListTailRecursive
  //
  // terminate successfully
  // Stream((0 to 1000000).toList: _*).toListTailRecursive

  println(Stream(0, 1, 2, 3, 4).take(3))
  println(Stream(0, 1, 2, 3, 4).take(3).toList)
  println(Stream(0, 1, 2, 3, 4).drop(2).toList)
  println(Stream(0, 1, 2, 3, 4).takeWhile(n => n <= 2).toList)
  println(Stream(0, 1, 2, 3, 4).takeWhile(n => n % 2 == 0).toList)

  // stack overflow error
  // println(Stream((0 to 10000).toList: _*).exists(n => n == 100000))
  // println(Stream((0 to 10000).toList: _*).existsViaFoldRight(n => n == 100000))
  // println(Stream((0 to 10000).toList: _*).existsViaFoldRight(n => n == 5000))
  println(Stream((0 to 10000).toList: _*).existsViaFoldRight(n => n == 1000))

  println(Stream((0 to 100000).toList: _*).forAll(n => n < 100000000))
  println(Stream((0 to 100000).toList: _*).forAll(n => n < 50000))
  println(Stream(0, 1, 2, 3, 4, 5).forAllViaFoldRight(n => n <= 5))
  // stack overflow error
  // println(Stream((0 to 10000).toList: _*).forAllViaFoldRight(n => n <= 10000))

  println(Stream(0, 1, 2, 3, 4, 5).takeWhileViaFoldRight(n => n <= 3).toList)
  // 這不會有 stack overflow error
  // 原因是 foldRight 處理好第一個元素後立即停止並回傳
  // 尾部都被封印在 foldRight 裡面並未被立即求值
  // 嚴格說起來，其實這個運算式的 stream 回傳值
  // 的 head 和 tail 都各自被包在一個函數裡，尚未被求值
  Stream((0 to 100000).toList: _*).takeWhileViaFoldRight(n => n <= 100000)
  // stack overflow error
  // 對運算式結果 stream 呼叫 toList
  // 就會對 stream 所有元素求值
  // 可是因為 stream 的第二個、第三個、第四個、...、第 n 個元素，分別被包在
  // 1 層 foldRight, 2 層 foldRight, 3 層 foldRight, ..., n - 1 層 foldRight 裡面
  // 對它們求值，元素過多的話，就會 stack overflow 了
  // Stream((0 to 100000).toList: _*).takeWhileViaFoldRight(n => n <= 100000).toList

  println(Stream(0, 1, 2, 3, 4, 5).headOptionViaFoldRight)
  println(Stream[Int]().headOptionViaFoldRight)

  println(Stream(0, 1, 2, 3, 4, 5).mapViaFoldRight(n => n + 1).toList)
  println(Stream[Int]().mapViaFoldRight(n => n + 1).toList)
  println(Stream((0 to 100000).toList: _*).mapViaFoldRight(n => n + 1))

  println(Stream(0, 1, 2, 3, 4, 5).filterViaFoldRight(n => n % 2 == 0).toList)
  println(Stream[Int]().filterViaFoldRight(n => n % 2 == 0).toList)
  println(Stream((0 to 10000).toList: _*).filterViaFoldRight(n => n % 2 == 0))

  println(Stream(0, 1, 2).append(Stream(3, 4, 5)).toList)
  println(Stream[Int]().append(Stream(0, 1, 2)).toList)
  println(Stream(0, 1, 2).append(Stream[Int]()).toList)

  println(Stream(0, 1, 2, 3, 4).flatMap(n => Stream(n, n)).toList)

  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones.take(5).toList)
  println(Stream.constant(2).take(4).toList)
  println(Stream.from(50).take(5).toList)
  println(Stream.fibs.take(10).toList)

  println(Stream.onesViaUnfold.take(5).toList)
  println(Stream.constantViaUnfold(99).take(3).toList)
  println(Stream.fromViaUnfold(10).take(3).toList)
  println(Stream.fibsViaUnfold().take(10).toList)

  println(Stream(0, 1, 2, 3, 4).mapViaFoldRight(n => n + 2).toList)
  println(Stream(0, 1, 2, 3, 4).takeViaUnfold(3).toList)
  println(Stream(0, 1, 2, 3, 4).takeViaUnfold(6).toList)
  println(Stream(0, 1, 2, 3, 4).takeWhileViaFoldRight(n => n <= 0).toList)
  println(Stream(0, 1, 2, 3, 4).takeWhileViaFoldRight(n => n <= 4).toList)
  println(
    Stream(0, 1, 2, 3, 4)
      .zipWithViaUnfold(Stream(5, 6, 7, 8, 9))((x, y) => x + y)
      .toList)

  println(Stream(0, 1, 2, 3).startsWith(Stream(0, 1, 2)))
  println(Stream(0, 1, 2).startsWith(Stream(0, 1, 2, 3)))
  println(Stream(0, 1, 2).startsWith(Stream()))
  println(Stream(1, 2, 3).tails.mapViaFoldRight(s => s.toList).toList)
}