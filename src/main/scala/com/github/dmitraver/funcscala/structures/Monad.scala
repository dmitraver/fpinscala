package com.github.dmitraver.funcscala.structures

import com.github.dmitraver.funcscala.random.State
import com.github.dmitraver.funcscala.structures.MonadApplication.IntState


// associativity law: x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
trait Monad[F[_]] extends Functor[F]{
  def unit[A](a: => A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    compose((_: Unit) => fa, f)()
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
    sequence(List.fill(n)(ma))
  }

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
    case Nil => unit(Nil)
    case x :: xs => flatMap(f(x))(b => if (b) map(filterM(xs)(f))(y => x :: y) else filterM(xs)(f))
  }

  // a => F[B] - Kleisli arrow
  // compose(compose(f, g), h) == compose(f, compose(g, h))
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(b => g(b))
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(a => a)
  }

  def flatMapViaJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    join(map(ma)(f))
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

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object IdMonad extends Monad[Id] {
  override def unit[A](a: => A): Id[A] = Id(a)
  override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = fa.flatMap(f)
}

object IntStateMonad extends Monad[IntState] {
  override def unit[A](a: => A): IntState[A] = State(s => (a, s))
  override def flatMap[A, B](fa: IntState[A])(f: (A) => IntState[B]): IntState[B] = fa flatMap f
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(r => a)
    override def flatMap[A, B](fa: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] = {
      Reader { r =>
        val a = fa.run(r)
        val b = f(a)
        b.run(b)
      }
    }
  }
}


object MonadApplication {

  type IntState[A] = State[Int, A]

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
  }

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
    println("Replicate list 1: " + ListMonad.replicateM(5, List(1, 2)))
    println("Replicate list: " + ListMonad.replicateM(5, Nil))

    // filterM
    println(OptionMonad.filterM(List(1, 2, 3, 4, 5, 6))(x => Some(x % 2 == 0)))
    println(ListMonad.filterM(List(1, 2))(x => List(true, false)))

    val result = for {
      a <- Id("aaa")
      b <- Id("bbb")
    } yield a + b

    println(result)

    val value = stateMonad[Int].replicateM(5, new State[Int, Int](s => (s, s + 1)))
    println(value.run(1))
  }
}

