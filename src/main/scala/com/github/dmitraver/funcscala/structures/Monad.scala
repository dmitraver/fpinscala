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

  def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
    case Nil => unit(Nil)
    case x :: xs => flatMap(x)(a => map(sequence(xs))(b => a :: b))
  }

  def traverse[A, B](lma: List[A])(f: A => F[B]): F[List[B]] = lma match {
    case Nil => unit(Nil)
    case x :: xs => flatMap(f(x))(a => map(traverse(xs)(f))(b => a :: b))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    if (n == 0) unit(Nil)
    else {
      flatMap(ma)(a => map(replicateM(n - 1, ma))(b => a :: b))
    }
  }

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case x :: xs => flatMap(f(x))(b => if (b) map(filterM(xs)(f))(y => x :: y) else filterM(xs)(f))
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

object MonadApplication {
  def main(args: Array[String]): Unit = {
    // sequence
    println(OptionMonad.sequence(List(Some(1), Some(2), Some(3))))
    println(OptionMonad.sequence(List(Some(1), None, Some(3))))

    // traverse
    println(OptionMonad.traverse(List(1, 2, 3))(a => Some(a)))
    println(OptionMonad.traverse(List(1, 2, 3))(a => if (a == 2) None else Some(a)))

    //replicate option
    println(OptionMonad.replicateM(5, Some(1)))
    println(OptionMonad.replicateM(5, None))

    //replicate list
    println(ListMonad.replicateM(5, List(1, 2)))
    println(ListMonad.replicateM(5, Nil))

    // filterM
    println(OptionMonad.filterM(List(1, 2, 3, 4, 5, 6))(x => Some(x % 2 == 0)))
  }
}

