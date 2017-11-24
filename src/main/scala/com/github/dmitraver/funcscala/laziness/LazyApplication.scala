package com.github.dmitraver.funcscala.laziness

import Stream._

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) if n > 0 =>
      cons(h(), t().take(n - 1))
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case a @ Cons(h, t) => a
  }

  def dropWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => t().dropWhile(p)
    case a @ Cons(h, t) => a
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if !p(h()) => false
    case Cons(h, t) => t().forAll(p)
    case Empty => true
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  def headOptionViaFold: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => cons(f(h()), t().map(f))
    case Empty => empty
  }

  def find(p: A => Boolean): Option[A] = {
    filter(p).headOption
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Cons(h, t) => f(h()).append(t().flatMap(f))
    case Empty => empty
  }

  def append[B >: A](that: => Stream[B]): Stream[B] = this match {
    case Cons(h, t) => cons(h(), t().append(that))
    case Empty => that
  }

  def appendViaFoldRight[B >: A](that: => Stream[B]): Stream[B] = {
    foldRight(that)((value, stream) => cons(value, stream))
  }

  def filter(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().filter(p))
    case Cons(h, t) => t().filter(p)
    case Empty => empty
  }

  def mapViaFoldRight[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((0, this)) {
      case (i, Cons(h, t)) if i == n => None
      case (_, Empty) => None
      case (i, Cons(h, t)) => Some((h(), (i + 1, t())))
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if !p(h()) => None
      case Cons(h, t) => Some((h(), t()))
      case Empty => None
    }
  }

  def filterViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if(!p(a)) cons(a, b) else b)
  }

  def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((head, stream) => f(head).appendViaFoldRight(stream))
  }

  def zipWith[B, C](that: Stream[B]) (f: (A, B) => C): Stream[C] = {
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.isDefined).forAll(a => a._1 == a._2)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case a @ Cons(h, t) => Some((a, t()))
      case Empty => None
    }
  }

  def scanRight[B](z: B) (f: (A, => B) => B): Stream[B] = this match {
    case a @ Cons(h, t) =>
      val tail = t().scanRight(z)(f)
      cons(a.foldRight(z)(f), tail)
    case Empty => cons(z, empty)
  }

  def scanRightViaFoldRight[B](z: B) (f: (A, B) => B): Stream[B] = {
    foldRight((z, cons(z, empty))) { (a, b) =>
      val result = f(a, b._1)
      (result, cons(result, b._2))
    }._2
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean = {
    tails exists (_ startsWith s)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(i => Some(i, i + 1))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(i => Some(i, i))
  }

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _ *))
  }

  def unfold[A, S](z:S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, b)) => cons(a, unfold(b)(f))
      case None => empty
    }
  }
}

object App {

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))
    }

    go(0, 1)
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) {
      case (a, b) => Some((a, (b, b + a)))
    }
  }



  def main(args: Array[String]): Unit = {
    /*val stream = Stream(1, 2, 3, 4, 5)
    println("Take")
    println(stream.take(0).toList)
    println(stream.take(1).toList)
    println(stream.take(2).toList)
    println(stream.take(10).toList)

    println("Drop")
    println(stream.drop(0).toList)
    println(stream.drop(1).toList)
    println(stream.drop(2).toList)
    println(stream.drop(10).toList)*/

    //val stream = Stream(println(1), println(2), println(3))
    //val stream = Cons(() => println(1), () => Cons(() => println(2), () => Empty))
    //println(stream.drop(2))

    //    val stream = Stream.from(1)
    //    println(stream.dropWhile(_ < 40).headOption)
    //
    //    println(Stream(2, 4, 6, 8).forAll(_ % 2 == 0))
    //    println(Stream(1, 2 , 3).forAll(_ % 2 == 0))

    /*val stream = Stream.from(1)
    println(stream.takeWhile(_ < 10).toList)*/

    /*val stream = Stream(1, 2, 3, 4, 5, 6)
    println(stream.filter(_ < 4).toList)*/

    val stream = Stream(1, 2, 3, 4, 5, 6)
    println(stream.mapViaFoldRight(_ * 2).toList)
    println(stream.map(_ * 2).toList)
    println(stream.filterViaFoldRight(_ % 2 == 0).toList)
    println(stream.filter(_ % 2 == 0).toList)
    println(stream.flatMapViaFoldRight(_ => Stream(1, 2, 3)).toList)
    println(stream.flatMap(_ => Stream(1, 2, 3)).toList)
    println(stream.appendViaFoldRight(Stream(7, 8, 9)).toList)
    println(stream.append(Stream(7, 8, 9)).toList)

    def ones: Stream[Int] = Stream.constant(2)
    println(ones.take(5).toList)

    println(fibs.take(10).toList)

    println(unfold(0) (i => Some((i, i))).take(5).toList)
    println(unfold(10) (i => Some((i, i + 1))).take(5).toList)
    println(fibsViaUnfold.take(10).toList)

    println(stream.mapViaUnfold(_ * 2).toList)
    println(stream.takeViaUnfold(10).toList)
    println(stream.takeViaUnfold(0).toList)
    println(stream.takeViaUnfold(2).toList)

    println(stream.takeWhile(_ < 4).toList)
    println(stream.zipWith(Stream(7, 8, 9))(_ + _).toList)
    println(stream.zipAll(Stream(7, 8, 9)).toList)
    println(stream.startsWith(Stream(1, 2, 3, 4, 5, 6)))
    println(stream.startsWith(Stream(1, 2, 3, 4, 5, 7)))
    println(stream.hasSubsequence(Stream(1, 2)))
    println(stream.hasSubsequence(Stream(3, 4)))
    println(stream.hasSubsequence(Stream(6)))
    println(stream.hasSubsequence(Stream(7, 2)))

    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
    println(Stream(1, 2, 3).scanRightViaFoldRight(0)(_ + _).toList)
  }
}