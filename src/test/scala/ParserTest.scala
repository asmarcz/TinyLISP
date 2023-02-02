package asmar.tinylisp

import parser.*

import org.scalatest.Outcome
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.*
import org.scalatest.matchers.should.Matchers.*

class ParserTest extends AnyFunSuite {
  test("nil") {
    item()("nil") should equal(Accept(IdentifierItem("nil"), ""))
  }

  test("binOps") {
    val symbols = List('+', '-', '*', '/', '<', '>', '=')
    for (symbol <- symbols.map(_.toString)) {
      item()(symbol) should equal(Accept(IdentifierItem(symbol), ""))
    }
  }

  test("int") {
    item()("123") should equal(Accept(IntItem(123), ""))
  }

  test("double") {
    item()("0.050378325") should equal(Accept(DoubleItem(0.050378325), ""))
  }

  test("cons sugar") {
    item()("(3 . 4)") should equal(Accept(ConsItem(IntItem(3), IntItem(4)), ""))
  }

  test("application") {
    item()("(func x 3)") should equal(Accept(ListItem(List(
      IdentifierItem("func"), IdentifierItem("x"), IntItem(3)
    )), ""))
  }

  test("brackets") {
    val brackets = List(('(', ')'), ('[', ']'))
    for (p <- brackets) {
      item()(p._1 + "func x y" + p._2) should equal(Accept(ListItem(List(
        IdentifierItem("func"), IdentifierItem("x"), IdentifierItem("y"))
      ), ""))
    }
  }

  test("quote") {
    item()("'(1 2 3)") should matchPattern { case Accept(QuotedItem(ListItem(_)), "") => }
    item()("'ident") should matchPattern { case Accept(QuotedItem(IdentifierItem(_)), "") => }
  }
}

