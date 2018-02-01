package com.github.dmitraver.funcscala.structures

trait Traverse[F[_]] {

  // traverse[A, B] (as: List[A])(f: A => F[B]): F[List[B]]
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
    sequence(map(fa)(f))
  }

  // sequence[A](fas: List[F[A]]): F[List[A]]
  def sequence[G[_]:Applicative, A](fga: F[G[A]]): G[F[A]] = {
    traverse(fga)(ga => ga)
  }
}

object ListTraverse extends Traverse[List] {
  override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] = {
    val AG = implicitly[Applicative[G]]
    fa.foldRight(AG.unit(Nil: List[B]))((a, acc) => AG.map2(f(a), acc)(_ :: _))
  }
}

object OptionTraverse extends Traverse[Option] {
  override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
    val AG = implicitly[Applicative[G]]
    fa.foldRight(AG.unit(None: Option[B]))( (a, acc) => AG.map2(f(a),acc)((a, b) => Some(a)))
  }
}
