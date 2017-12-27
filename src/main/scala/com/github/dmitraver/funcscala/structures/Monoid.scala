package com.github.dmitraver.funcscala.structures

trait Monoid[A] {
  def op(a1: A, a2: A): A // satisfies op(x, op(y, z)) == op(op(x, y), z)
  def zero: A // satisfies op(x, zero) == x and op(zero, x) == x
}

class StringMonoid extends Monoid[String] {
  override def op(a1: String, a2: String): String = a1 + a2
  override def zero: String = ""
}

class ListMonoid[A] extends Monoid[List[A]] {
  override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  override def zero: List[A] = Nil
}

class IntAdditionMonoid extends Monoid[Int] {
  override def op(a1: Int, a2: Int): Int = a1 + a2
  override def zero: Int = 0
}

class IntMultiplicationMonoid extends Monoid[Int] {
  override def op(a1: Int, a2: Int): Int = a1 * a2
  override def zero: Int = 1
}

class BooleanOrMonoid extends Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  override def zero: Boolean = false
}

class BooleanAndMonoid extends Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  override def zero: Boolean = true
}

class OptionMonoid[A] extends Monoid[Option[A]] {
  override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
  override def zero: Option[A] = None
}

class EndoMonoid[A] extends Monoid[A => A] {
  override def op(a1: (A) => A, a2: (A) => A): (A) => A = a => a2(a1(a))
  override def zero: (A) => A = a => a
}

object Application {
  def concatenate[A](list: List[A], monoid: Monoid[A]): A = {
    list.foldLeft(monoid.zero)(monoid.op)
  }

  def foldMap[A, B](list: List[A], monoid: Monoid[B])(f: A => B): B = {
    list.map(f).foldLeft(monoid.zero)(monoid.op)
  }

  def foldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    foldMap(list, new EndoMonoid[B])(f.curried)(z)
  }

  def main(args: Array[String]): Unit = {
    val words = List("Hi", "Whats", "up", "dawg")
    val strMonoid = new StringMonoid
    println(words.foldLeft(strMonoid.zero)(strMonoid.op))
    println(words.foldRight(strMonoid.zero)(strMonoid.op))
    println(foldLeft(List(1,2,3,4), 0)(_ + _))
  }
}


