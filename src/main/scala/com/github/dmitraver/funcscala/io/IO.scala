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
  def PrintLine(msg: String): IO[Unit] = {
    new IO {
      override def run: Unit = println(msg)
    }
  }
}