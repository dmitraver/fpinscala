package com.github.dmitraver.funcscala.parser

import com.github.dmitraver.funcscala.parser.JSON._

import scala.language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  def char(c: Char): Parser[Char] = string(c.toString).map(_.head)
  def succeed[A](a: A): Parser[A]
  implicit def string(s: String): Parser[String]
  def orString(s1: String, s2: String): Parser[String] // "abra" or "cadabra" ?
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n == 0) succeed(Nil)
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]
  def many(c: Char): Parser[Int] = {
    //map(many(char(c)))(_.length)
    //char(c).many.map(_.length)
    char(c).many.slice.map(_.length)
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    // map2(p, many(p) | succeed(Nil)) ( _ :: _)
    map2(p, many(p)) ( _ :: _)  | succeed(Nil)
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((_, b) => b)
  }

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    p1.flatMap(a => p2.map(b => (a, b)))
  }

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    //(p1 ** p2) map {case (a, b) => f(a, b)}
    //(p1 ** p2) map f.tupled
    p1.flatMap(a => p2.map(b => f(a, b)))
  }

  def pair(p1: Parser[Char], p2: Parser[Char]): Parser[(Int, Int)]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
    p.flatMap(a => succeed(f(a)))
  }

  def onError[A](p: Parser[A], onError: A): Parser[A]

  def split[A](p: Parser[A], separator: Char): Parser[List[A]]
  def split[A, B](p: Parser[A], separator: Parser[B]): Parser[List[A]]

  // error handling
  def errorLocation(error: ParseError): ErrorLocation
  def errorMessage(error: ParseError): String
  def label[A](label: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOpts[String] = ParserOpts(f(a))
  implicit def regex(r: Regex): Parser[String]

  implicit class ParserOpts[A](p: Parser[A]) {
    def |[B >: A](p1: Parser[B]): Parser[B] = self.or(p, p1)
    def or[B >: A](p1: => Parser[B]): Parser[B] = self.or(p, p1)
    def many[B >: A]: Parser[List[B]] = self.many(p)
    def many1[B >: A]: Parser[List[B]] = self.many1(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](p1: Parser[B]): Parser[(A, B)] = self.product(p, p1)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def split(separator: Char): Parser[List[A]] = self.split(p, separator)
    def split[B](separator: Parser[B]): Parser[List[A]] = self.split(p, separator)
    def onError(a: A): Parser[A] = self.onError(p, a)
  }
}

case class ParseError(errors: List[(ErrorLocation, String)])
case class ErrorLocation(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

object Application {

  def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val letter = regex("[a-zA-Z]".r).map(_.head)
    val digit = regex("[0-9]".r).map(_.head.toInt)
    val space = char(' ')
    val spaces = space.many.slice
    val newLine = char('\n')

    val bool = ("true" or "false") map(b => JBool(b.toBoolean))
    val number = for {
      sign   <- char('-') onError '+'
      first  <- digit.many1.slice
      dot    <- string(".") onError ""
      second <- digit.many.slice
    } yield JNumber((sign + first + dot + second).toDouble)

    val comma = spaces ** char(',') ** (spaces or newLine)

    val newLine_ = spaces or newLine

    val null_ = string("null").map(_ => JNull)
    val str = for {
      _    <- char('"')
      str  <- letter.many.slice
      _    <- char('"')
    } yield JString(str)

    def array: Parser[JArray] = for {
      _     <- char('[') ** newLine_
      vals  <- (bool or number or null_ or str or array or obj).split(comma)
      _     <- newLine_ ** char(']')
    } yield JArray(vals.toVector)

    def kv: Parser[(String, JSON)] = for {
      key   <- str
      _     <- spaces ** char(':') ** spaces
      value <- bool or number or null_ or str or array or obj
    } yield key.get -> value

    def obj: Parser[JObject] = for {
      _     <- char('{') ** newLine_
      kvs   <- kv.split(comma)
      _     <- newLine_ ** char('}')
    } yield JObject(kvs.toMap)

    obj
  }

  def main(args: Array[String]): Unit = {
    // laws
    // for any char c | law run(char(c))(c.toString) == Right(c)
    // for any string s | law run(string(s))(s) == Right(s)
    // run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
    // run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
    // run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    // run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    // run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
    // run(zeroOrMoreTimes('a'))("aaacdvb") == Right(3)
    // run(zeroOrMoreTimes('a'))("baaddvb") == Right(0)
    // run(oneOrMoreTimes('a'))("aaddvb") == Right(3)
    // run(oneOrMoreTimes('a'))("baaddvb") == Left("Expected one or more 'a'")
    // map(p)(a => a) == p
    // run(succeed('a'))(s) == Right('a')
    // run(slice(a | b).many)("aabb") == Right("aabb")

    val parser = new ParserImpl
    import parser._
    val p = "hello" ** "world"
    val result = run(p)("helloworld").right.get
    println(result._1 + result._2)
  }
}
