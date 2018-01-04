package com.github.dmitraver.funcscala.structures

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
    }

    override def zero: WC = Stub("")
  }
}


