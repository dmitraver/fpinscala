package com.github.dmitraver.funcscala.io

import com.github.dmitraver.funcscala.structures.Monad

sealed trait TailRec[A] {
  def map[B](f: A => B): TailRec[B] = flatMap(f andThen(Return(_)))
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
}

case class Return[A](a: A) extends TailRec[A]
case class Suspend[A](resume: () => A) extends TailRec[A]
case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

  override def unit[A](a: => A): TailRec[A] = Return(a)
  override def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = fa flatMap f
}
