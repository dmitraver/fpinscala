package com.github.dmitraver.funcscala.structures

trait Traverse[F[_]] extends Functor[F]{

  // traverse[A, B] (as: List[A])(f: A => F[B]): F[List[B]]
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
    sequence(map(fa)(f))
  }

  // sequence[A](fas: List[F[A]]): F[List[A]]
  def sequence[G[_]:Applicative, A](fga: F[G[A]]): G[F[A]] = {
    traverse(fga)(ga => ga)
  }

  override def map[A, B](as: F[A])(f: (A) => B): F[B] = {
    implicit val id = Applicatives.idApplicative
    traverse(as) (a => id.unit(f(a))).value
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

case class Tree_[+A](head: A, tail: List[Tree_[A]])
object TreeTraverse extends Traverse[Tree_] {
  override def traverse[G[_] : Applicative, A, B](fa: Tree_[A])(f: (A) => G[B]): G[Tree_[B]] = {
    val AG = implicitly[Applicative[G]]
    AG.map2(f(fa.head), ListTraverse.traverse(fa.tail)(tree => traverse(tree)(f)))((a, b) => Tree_(a, b))
  }
}


object TraverseApplication {
  def main(args: Array[String]): Unit = {
    println(OptionTraverse.map(Some(10))(_ * 2))
    println(OptionTraverse.map(None: Option[Int])(_ * 2))
    println(ListTraverse.map(List(1, 2, 3, 4, 5))(_ * 2))
  }
}