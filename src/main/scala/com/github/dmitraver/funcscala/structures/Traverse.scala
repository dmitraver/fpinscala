package com.github.dmitraver.funcscala.structures

import com.github.dmitraver.funcscala.random.State

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  type Const[M, B] = M

  private def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
    override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = M.op(fa, fb)
    override def unit[A](a: => A): Const[M, A] = M.zero
  }

  // traverse[A, B] (as: List[A])(f: A => F[B]): F[List[B]]
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
    sequence(map(fa)(f))
  }

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(MonadApplication.stateMonad)

  // sequence[A](fas: List[F[A]]): F[List[A]]
  def sequence[G[_]:Applicative, A](fga: F[G[A]]): G[F[A]] = {
    traverse(fga)(ga => ga)
  }

  override def map[A, B](as: F[A])(f: (A) => B): F[B] = {
    implicit val id = Applicatives.idApplicative
    traverse(as) (a => id.unit(f(a))).value
  }

  override def foldMap[A, M](as: F[A])(f: (A) => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M,x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

// this two methods were initial attempts to come up with mapAccum
//  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = {
//    traverseS(fa) { a =>
//      for {
//        i  <- State.get[Int]
//        _  <- State.set(i + 1)
//      } yield (a, i)
//    }.run(0)._1
//  }
//
//  def toList[A](fa: F[A]): List[A] = {
//    traverseS(fa) { a =>
//      for {
//        l <- State.get[List[A]]
//        _ <- State.set(a :: l)
//      } yield ()
//    }.run(Nil)._2.reverse
//  }

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
    traverseS(fa){ a =>
      for {
        s1  <- State.get[S]
        (b, s2) = f(a, s1)
        _  <- State.set(s2)
      } yield b
    }.run(s)
  }

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = {
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1
  }

  def toList[A](fa: F[A]): List[A] = {
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse
  }

  def reverse[A](fa: F[A]): F[A] = {
    mapAccum(fa, toList(fa).reverse)((a, s) => (s.head, s.tail))._1
  }

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    mapAccum(as, z)((a, s) => ((), f(s, a)))._2
  }
}

object ListTraverse extends Traverse[List] {
  override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] = {
    val AG = implicitly[Applicative[G]]
    fa.foldRight(AG.unit(Nil: List[B]))((a, acc) => AG.map2(f(a), acc)(_ :: _))
  }

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???
}

object OptionTraverse extends Traverse[Option] {
  override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
    val AG = implicitly[Applicative[G]]
    fa.foldRight(AG.unit(None: Option[B]))( (a, acc) => AG.map2(f(a),acc)((a, b) => Some(a)))
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = ???
}

case class Tree_[+A](head: A, tail: List[Tree_[A]])
object TreeTraverse extends Traverse[Tree_] {
  override def traverse[G[_] : Applicative, A, B](fa: Tree_[A])(f: (A) => G[B]): G[Tree_[B]] = {
    val AG = implicitly[Applicative[G]]
    AG.map2(f(fa.head), ListTraverse.traverse(fa.tail)(tree => traverse(tree)(f)))((a, b) => Tree_(a, b))
  }

  override def foldRight[A, B](as: Tree_[A])(z: B)(f: (A, B) => B): B = ???
}


object TraverseApplication {
  def main(args: Array[String]): Unit = {
    println(OptionTraverse.map(Some(10))(_ * 2))
    println(OptionTraverse.map(None: Option[Int])(_ * 2))
    println(ListTraverse.map(List(1, 2, 3, 4, 5))(_ * 2))

    println(ListTraverse.reverse(List(1,2,3,4,5,6)))
    println(ListTraverse.foldLeft(List(1,2,3))(0)(_ + _))
  }
}