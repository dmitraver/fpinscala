package com.github.dmitraver.funcscala.structures

trait Applicative[F[_]] extends Functor[F] {

  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  override def map[A, B](as: F[A])(f: (A) => B): F[B] = {
    map2(as, unit(()))((a, b) => f(a))
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
