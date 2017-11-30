package com.github.dmitraver.funcscala.parser

import com.github.dmitraver.funcscala.parser.Parser.Parser

import scala.util.matching.Regex

class ParserImpl extends Parsers[Parser] {
  override implicit def string(str: String): Parser[String] = {
    (input, offset) =>
      if (input.startsWith(str, offset)) Right((str, offset + str.length))
      else Left(ParseError(List((ErrorLocation(input, offset), "Error")))) // TODO: make nice error handling
  }

  override def orString(s1: String, s2: String): Parser[String] = ???

  override def or[A](p1: Parser[A], p2: =>Parser[A]): Parser[A] = ???

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(input, 0).right.map{ case (value, offset) => value}
  }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = {
    (input, offset) =>
      val result = p(input, offset)
      result.right.flatMap {
        case (value, offset) => f(value)(input, offset)
      }
  }


  override def succeed[A](a: A): Parser[A] = {
    (input, offset) => Right((a, offset))
  }

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def pair(p1: Parser[Char], p2: Parser[Char]): Parser[(Int, Int)] = ???

  override def onError[A](p: Parser[A], onError: A): Parser[A] = ???

  override def split[A](p: Parser[A], separator: Char): Parser[List[A]] = ???

  override def split[A, B](p: Parser[A], separator: Parser[B]): Parser[List[A]] = ???

  override def errorLocation(error: ParseError): ErrorLocation = ???

  override def errorMessage(error: ParseError): String = ???

  override def label[A](label: String)(p: Parser[A]): Parser[A] = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override implicit def regex(r: Regex): Parser[String] = ???
}

object Parser {
  type Offset = Int
  type Parser[+A] = (String, Offset) => Either[ParseError, (A, Offset)] // use State type here?
}
