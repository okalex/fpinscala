package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    @annotation.tailrec
    def go(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(x, acc))
    }
    go(l, z)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(a, as) => Cons(f(a), map(as)(f))
  }

  def mapLeft[A, B](l: List[A])(f: A => B): List[B] = {
    reverse(foldLeft[A, List[B]](l, Nil)((x, xs) => Cons(f(x), xs)))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  def filterLeft[A](as: List[A])(f: A => Boolean): List[A] = {
    reverse(foldLeft[A, List[A]](as, Nil) { (x, xs) =>
      if (f(x)) Cons(x, xs) else xs
    })
  }

  def filterFromFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) { x =>
      if (f(x)) Cons(x, Nil) else Nil
    }
  }

  def flatten[A](ls: List[List[A]]): List[A] =  {
    foldRight[List[A], List[A]](ls, Nil) {
      (l, acc) => foldRight(l, acc)(Cons(_, _))
    }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil
    case (Cons(_, _), 0) => l
    case (Cons(_, xs), _) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f)
                        else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, acc) => acc + 1)

  def sumLeft(ints: List[Int]) = foldLeft(ints, 0)(_ + _)

  def productLeft(ints: List[Int]) = foldLeft(ints, 1)(_ * _)

  def lengthLeft[A](as: List[A]) = foldLeft(as, 0)((_, acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)(Cons(_, _))

  def foldLeftFromRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldRight[A, B](as, z) { (x, xs) => 
    f(x, xs)
  }

  def foldRightFromLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft[A, B](reverse(as), z) { (x, xs) =>
    f(x, xs)
  }

  def appendFold[A](a1: List[A], a2: List[A]) = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def concat[A](ass: List[List[A]]) = {
    foldRight[List[A], List[A]](ass, Nil) {
      (l, acc) => foldRight(l, acc)(Cons(_, _))
    }
  }

  def concatLeft[A](ass: List[List[A]]) = {
    reverse(foldLeft[List[A], List[A]](ass, Nil) {
      (l, acc) => foldLeft(l, acc)(Cons(_, _))
    })
  }

  def addOne(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def doublesToString(ds: List[Double]): List[String] = ds match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doublesToString(xs))
  }

  def removeOdds(ints: List[Int]) = filter(ints)(_ % 2 == 0)
  def removeOddsLeft(ints: List[Int]) = filterLeft(ints)(_ % 2 == 0)
}

object ListModule {
  import List._

  def main(args: Array[String]): Unit = {
    println(s"List.x = ${List.x}")

    val l = List(1, 2, 3, 4, 5)
    val l2 = List(6, 7, 8, 9, 10)
    val l3 = List(11, 12, 13, 14, 15)
    val l4 = List(16, 17, 18, 19, 20)
    val ls = List(l, l2, l3, l4)

    val ds = List[Double](1.0, 2.0, 3.0, 4.0, 5.0)

    println(s"l = $l")

    println(s"tail(l) = ${tail(l)}")
    println(s"drop(l, 3) = ${drop(l, 3)}")
    println(s"dropWhile(l, x < 5) = ${dropWhile(l, (x: Int) => x < 5)}")
    println(s"init(l) = ${init(l)}")
    println(s"length(l) = ${length(l)}")
    println(s"foldLeft(l, 0)(_ + _) = ${foldLeft(l, 0){ (x, a) => println(s"eval $x: ${a + x}"); a + x }}")
    println(s"sumLeft(l) = ${sumLeft(l)}")
    println(s"productLeft(l) = ${productLeft(l)}")
    println(s"lengthLeft(l) = ${lengthLeft(l)}")
    println(s"reverse(l) = ${reverse(l)}")
    println(s"foldLeftFromRight(l, 0)(_ + _) = ${foldLeftFromRight(l, 0){ (x, a) => println(s"eval $x: ${a + x}"); a + x }}")
    println(s"foldRightFromLeft(l, 0)(_ + _) = ${foldRightFromLeft(l, 0){ (x, a) => println(s"eval $x: ${a + x}"); a + x }}")

    println(s"append(l, l2) = ${append(l, l2)}")
    println(s"appendFold(l, l2) = ${appendFold(l, l2)}")
    println(s"concat(ls) = ${concat(ls)}")
    println(s"concatLeft(ls) = ${concatLeft(ls)}")

    println(s"addOne(l) = ${addOne(l)}")
    println(s"doublesToString(ds) = ${doublesToString(ds)}")
    println(s"map(l)(_ + 1) = ${map(l)(_ + 1)}")
    println(s"mapLeft(l)(_ + 1) = ${mapLeft(l)(_ + 1)}")

    println(s"removeOdds(l) = ${removeOdds(l)}")
    println(s"removeOddsLeft(l) = ${removeOddsLeft(l)}")

    println(s"flatMap(l)(x => List(x + 1, x + 2, x + 3)) = ${flatMap(l)(x => List(x + 1, x + 2, x + 3))}")
    println(s"filterFromFlatMap(l)(_ % 2 == 0) = ${filterFromFlatMap(l)(_ % 2 == 0)}")

  }
}
