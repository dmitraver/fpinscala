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

  def apply[A](a: => A): IO[A] = unit(a)
}

object IOApplication {
  def printLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  def readLine: IO[String] = IO {
    scala.io.StdIn.readLine()
  }

  def greet: IO[Unit] = for {
    _      <- printLine("Please enter your name")
    name   <- readLine
    _      <- printLine(s"Thank you $name!")
  } yield ()

  def main(args: Array[String]): Unit = {
    val echo = readLine flatMap printLine
    val readInt = readLine map(_.toInt)
    val ints = IO.replicateM(2, readInt).run
    println(ints)

  }


}