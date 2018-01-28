package com.github.dmitraver.funcscala.structures

trait Applicative[F[_]] extends Functor[F] {

  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fa, fab)((a, ab) => ab(a))
  }

  override def map[A, B](as: F[A])(f: A => B): F[B] = {
    map2(as, unit(()))((a, b) => f(a))
  }

  // via apply and unit
  def map_[A, B](as: F[A])(f: A => B): F[B] = {
    apply(unit(f))(as)
  }

  // via apply and unit
  def map2_[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fbc = apply(unit(f.curried))(fa)
    apply(fbc)(fb)
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fbcd = apply(unit(f.curried))(fa)
    val fcd = apply(fbcd)(fb)
    apply(fcd)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fbcde = apply(unit(f.curried))(fa)
    val fcde = apply(fbcde)(fb)
    val fde = apply(fcde)(fc)
    apply(fde)(fd)
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }

  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    traverse(fas)(a => a)
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((_, _))
  }
}

object ApplicativeApplication {
  def main(args: Array[String]): Unit = {

  }
}
