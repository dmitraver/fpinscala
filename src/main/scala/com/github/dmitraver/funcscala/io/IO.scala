package com.github.dmitraver.funcscala.io

import com.github.dmitraver.funcscala.structures.Monad

trait IO[A] { self =>
  def run: A
  def ++(io: IO[A]): IO[A] = new IO[A] {
    override def run: A = {
      self.run
      io.run
    }
  }

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run: B = f(self.run)
  }

  def flatMap[B](f: (A) => IO[B]): IO[B] = f(self.run)
}

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = f(fa.run)

  def empty = new IO[Unit] {
    override def run: Unit = ()
  }
}

object IOApplication {
  def printLine(msg: String): IO[Unit] = {
    new IO[Unit] {
      override def run: Unit = println(msg)
    }
  }

  def readLine: IO[String] = {
    IO.unit(scala.io.StdIn.readLine())
  }

  def main(args: Array[String]): Unit = {
    val greet: IO[Unit] = for {
      _      <- printLine("Please enter your name")
      name   <- readLine
      _      <- printLine(s"Thank you $name!")
    } yield ()

    greet.run
  }


}