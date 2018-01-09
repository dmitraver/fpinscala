package com.github.dmitraver.funcscala.structures

trait Functor[F[_]] {
  def map[A, B](as: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(a) => map(a)(Left(_))
    case Right(b) => map(b)(Right(_))
  }
}

object ListFunctor extends Functor[List] {
  override def map[A, B](as: List[A])(f: A => B): List[B] = as map f
}


