package com.github.dmitraver.funcscala.io

import com.github.dmitraver.funcscala.structures.Monad

import scala.io.StdIn

object Free {
  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] = {
      flatMap(f andThen(Return(_)))
    }
  }

  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]


  object FreeMonad {
    def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] = new Monad[({type f[x] = Free[F, x]})#f] {
      override def unit[A](a: => A): Free[F, A] = Return(a)
      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
    }
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(y, g) => runTrampoline(y flatMap(a => g(a) flatMap f))
    }
  }

  @annotation.tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(Return(a), f) => step(f(a))
    case FlatMap(FlatMap(x, f), g) => step(x flatMap(a => f(a) flatMap g))
    case _ => free
  }

  // this function can't be tail recursive due to FlatMap(Suspend(...), f) case
  def run[F[_], A](free: Free[F, A])(implicit M: Monad[F]): F[A] = step(free) match {
    case Return(a) => M.unit(a)
    case Suspend(fa) => fa
    case FlatMap(x, f) => x match {
      case Suspend(fa) => M.flatMap(fa)(a => run(f(a)))
      case _ => sys.error("step should eliminate this case")
    }
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~ G)(implicit M: Monad[G]): G[A] = step(free) match {
    case Return(a) => M.unit(a)
    case Suspend(fa) => t(fa)
    case FlatMap(Suspend(fa), f) => M.flatMap(t(fa))(a => runFree(f(a))(t))
    case _ => sys.error("step should eliminate this case")
  }

  sealed trait Console[A] {
    def toThunk: () => A
  }

  case object ReadLine extends Console[String] {
    override def toThunk: () => String = () => StdIn.readLine()
  }

  case class PrintLine(line: String) extends Console[Unit]  {
    def toThunk = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLine: ConsoleIO[String] = Suspend(ReadLine)
    def printLine(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  trait Translate[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  type ~[F[_], G[_]] = Translate[F, G]



  def main(args: Array[String]): Unit = {

    val consoleToFunc0 = new (Console ~ Function0) {
      override def apply[A](fa: Console[A]): () => A = fa.toThunk
    }

    implicit val func0Monad = new Monad[Function0] {
      override def unit[A](a: => A): () => A = () => a
      override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B =
        () => f(fa())()
    }

    val programm: Free[Console, String] = for {
      _    <- Console.printLine("What is your name?")
      name <- Console.readLine
    } yield name

    val res = runFree(programm)(consoleToFunc0)
    println(res())
  }
}