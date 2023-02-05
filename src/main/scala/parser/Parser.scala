package asmar.tinylisp
package parser

import util.*

import scala.annotation.targetName

// Inspired by: https://courses.fit.cvut.cz/BI-PPA/lectures/files/ppa2021-13.pdf

trait Parser[+A] extends (String => Result[A]) {
  def flatMap[B](f: A => Parser[B]): Parser[B] =
    (input: String) => this (input) match {
      case Accept(value, rem) => f(value)(rem)
      case r: Reject => r
    }

  def map[B](f: A => B): Parser[B] =
    this.flatMap(x => success(f(x)))

  @targetName("and")
  def ~[B](p: Parser[B]): Parser[(A, B)] =
    and(this, p)
}

def and[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] =
  for (v1 <- p1; v2 <- p2) yield (v1, v2)

def chr(c: Char): Parser[Char] =
  input => {
    if (input.isEmpty) Reject("End of input.")
    else if (input.head == c) Accept(c, input.tail)
    else Reject(s"Expected '$c', got \"$input\".")
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

val symbols = List('+', '-', '*', '/', '<', '>', '=')

def cons(): Parser[(Item, Item)] =
  input => enclosed(
    item() ~ forceWhitespace() ~ whitespace(chr('.')) ~ forceWhitespace() ~ whitespace(item())
  )(input) match {
    case Accept(value, rem) => Accept((value._1._1._1._1, value._2), rem)
    case r: Reject => r
  }

def double(): Parser[Double] =
  input => {
    val (num, rem) = input.span(c => c.isDigit || c == '.')
    if (num.nonEmpty && num.toDoubleOption.nonEmpty && num.contains('.')) Accept(num.toDouble, rem)
    else Reject("Expected fixed point number.")
  }

def enclosed[A](p: Parser[A]): Parser[A] =
  input => {
    if (input.isEmpty) {
      Reject("Expected bracket, got empty string.")
    } else {
      val bracket = input.head
      val matching = bracket match {
        case '(' => ')'
        case '[' => ']'
        case _ => ' '
      }
      if (matching == ' ')
        Reject(s"Expected opening bracket, got '$bracket'.")
      else (chr(bracket) ~ whitespace(p) ~ whitespace(chr(matching)))(input) match {
        case Accept(value, rem) => Accept(value._1._2, rem)
        case r: Reject => r
      }
    }
  }

def integer(): Parser[Int] =
  input => {
    val (num, rem) = input.span(c => c.isDigit)
    if (num.nonEmpty) Accept(num.toInt, rem)
    else Reject(s"Expected digit, got '${if (input.nonEmpty) input.head}'.")
  }

def item(): Parser[Item] =
  input1 => {
    quoted(
      input2 => {
        double()(input2) match {
          case Accept(value, rem) => Accept(DoubleItem(value), rem)
          case Reject(_) => integer()(input2) match {
            case Accept(value, rem) => Accept(IntItem(value), rem)
            case Reject(_) => or(identifier(), symbol().map(_.toString))(input2) match {
              case Accept(value, rem) => Accept(IdentifierItem(value), rem)
              case Reject(_) => cons()(input2) match {
                case Accept(value, rem) => Accept(ConsItem(value), rem)
                case Reject(_) => list()(input2) match {
                  case Accept(value, rem) => Accept(ListItem(value), rem)
                  case r: Reject => r
                }
              }
            }
          }
        }
      }
    )(input1)
  }

val identifierCharacters = List('!', '?', '-')

def identifier(): Parser[String] =
  input => {
    if (input.nonEmpty && input.head.isLetter)
      val (identifier, rem) = input.span(c => !c.isWhitespace && (c.isLetterOrDigit || identifierCharacters.contains(c)))
      Accept(identifier, rem)
    else
      Reject(s"Identifier cannot start with '${if (input.nonEmpty) input.head}'.")
  }

def list(): Parser[List[Item]] =
  input => enclosed(rep(whitespace(item())))(input) match {
    case Accept(value, rem) => Accept(value, rem)
    case r: Reject => r
  }

def quoted(p: Parser[Item]): Parser[Item] =
  input => {
    chr('\'')(input) match {
      case Accept(_, rem1) => p(rem1) match {
        case Accept(value, rem2) => Accept(QuotedItem(value), rem2)
        case r: Reject => r
      }
      case Reject(_) => p(input)
    }
  }

def symbol(): Parser[Char] =
  input =>
    if (input.nonEmpty && symbols.contains(input.head)) Accept(input.head, input.tail)
    else Reject(s"Expected symbol, got ${if (input.nonEmpty) input.head}")

def whitespace[A](p: Parser[A]): Parser[A] =
  input => p(input.dropWhile(c => c.isWhitespace))

def forceWhitespace(): Parser[Char] =
  input => input.head match
    case c if c.isWhitespace => Accept(c, input.tail)
    case _ => Reject(s"Expected whitespace, got '${input.head}' instead.")
