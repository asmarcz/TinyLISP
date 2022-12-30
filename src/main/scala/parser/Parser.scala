package asmar.tinylisp
package parser

// Inspired by: https://courses.fit.cvut.cz/BI-PPA/lectures/files/ppa2021-13.pdf

trait Parser[+A] extends (String => Result[A]) {
  def flatMap[B](f: A => Parser[B]): Parser[B] =
    (input: String) => this (input) match {
      case Accept(value, rem) => f(value)(rem)
      case r: Reject => r
    }

  def map[B](f: A => B): Parser[B] =
    this.flatMap(x => success(f(x)))
}

def and[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] =
  for (v1 <- p1; v2 <- p2) yield (v1, v2)

def chr(c: Char): Parser[Char] =
  input => {
    if (input.isEmpty) Reject("End of input.")
    else if (input.head == c) Accept(c, input.tail)
    else Reject(s"Expected $c, got $input.")
  }

def opt[A](p: Parser[A]): Parser[Option[A]] =
  or(p.map(x => Some(x)), success(None))

def or[A](p1: Parser[A], p2: Parser[A]): Parser[A] =
  input => p1(input) match {
    case Accept(value, rem) => Accept(value, rem)
    case Reject(_) => p2(input)
  }

def rep[A](p: Parser[A]): Parser[List[A]] =
  or(rep1(p), success(List()))

def rep1[A](p: Parser[A]): Parser[List[A]] =
  input => p(input) match {
    case Accept(head, rem1) => rep(p)(rem1) match {
      case Accept(tail, rem2) => Accept(head :: tail, rem2)
      case r: Reject => r
    }
    case r: Reject => r
  }

def success[A](value: A): Parser[A] =
  input => Accept(value, input)
