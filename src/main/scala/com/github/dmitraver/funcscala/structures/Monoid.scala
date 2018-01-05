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

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) m.zero
    else if (v.size == 1) f(v.head)
    else {
      val middle = v.size / 2
      val (left, right) = v.splitAt(middle)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
      override def zero: (A, B) = (a.zero, b.zero)
    }
  }

  def mapMergeMonoid[K, V] (V: Monoid[V]): Monoid[Map[K, V]] = {
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
        }
      override def zero: Map[K, V] = Map[K, V]()
    }
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B =
        a =>
          B.op(a1(a), a2(a))
      override def zero: A => B = a => B.zero
    }
  }

  def isOrdered(seq: IndexedSeq[Int]): Boolean = {
    val monoid = new Monoid[(Int, Boolean)] {
      override def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) = {
        if (!a2._2) (a1._1, false) else (a1._1, a1._1 <= a2._1)
      }

      override def zero: (Int, Boolean) = (Int.MaxValue, true)
    }

    foldMapV(seq, monoid)(a => (a, true))._2
  }

  def main(args: Array[String]): Unit = {
    val words = List("Hi", "Whats", "up", "dawg")
    val strMonoid = new StringMonoid
    println(words.foldLeft(strMonoid.zero)(strMonoid.op))
    println(words.foldRight(strMonoid.zero)(strMonoid.op))
    println(foldLeft(List(1,2,3,4), 0)(_ + _))
    println(foldMapV(Vector[String](), new IntAdditionMonoid)(x => x.toInt))
    println(isOrdered(Vector(1, 2, 3)))
    println(isOrdered(Vector(1, 2, 1)))
    println(isOrdered(Vector(1)))
    println(isOrdered(Vector(3, 2, 1)))
  }
}


