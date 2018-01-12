package com.github.dmitraver.funcscala.structures

trait Monad[F[_]] extends Functor[F]{
  def unit[A](a: => A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb)(b => f(a,b)))
  }
}

object OptionMonad extends Monad[Option] {
  override def unit[A](a: => A): Option[A] = Some(a)
  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
}

object StreamMonad extends Monad[Stream] {
  override def unit[A](a: => A): Stream[A] = Stream(a)
  override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa flatMap f
}

object ListMonad extends Monad[List] {
  override def unit[A](a: => A): List[A] = List(a)
  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
}

