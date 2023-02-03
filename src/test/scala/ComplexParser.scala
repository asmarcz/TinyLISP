package asmar.tinylisp

import parser.*

import org.scalatest.Outcome
import org.scalatest.funsuite.FixtureAnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class ComplexParser extends FixtureAnyFunSuite {
  override protected type FixtureParam = Parser[List[Item]]

  override protected def withFixture(test: OneArgTest): Outcome = {
    val parser = rep(whitespace(item()))
    try test(parser)
    finally {}
  }

  test("define null?") { parser =>
    parser(
      """(define (null? x)
        |  (= x nil))""".stripMargin
    ) should equal(Accept(List(ListItem(
      IdentifierItem("define"), ListItem(IdentifierItem("null?"), IdentifierItem("x")),
      ListItem(IdentifierItem("="), IdentifierItem("x"), IdentifierItem("nil"))
    )), ""))
  }

  test("define my-append") { parser =>
    parser(
      """(define (my-append it lst)
        |  (if (null? lst)
        |    (cons it nil)
        |    (let (left) ((car lst))
        |      (cons left (my-append it (cdr lst))))))""".stripMargin
    ) should equal(Accept(List(ListItem(
      IdentifierItem("define"), ListItem(IdentifierItem("my-append"), IdentifierItem("it"), IdentifierItem("lst")),
      ListItem(IdentifierItem("if"), ListItem(IdentifierItem("null?"), IdentifierItem("lst")),
        ListItem(IdentifierItem("cons"), IdentifierItem("it"), IdentifierItem("nil")),
        ListItem(IdentifierItem("let"), ListItem(IdentifierItem("left")),
          ListItem(ListItem(IdentifierItem("car"), IdentifierItem("lst"))),
          ListItem(IdentifierItem("cons"), IdentifierItem("left"),
            ListItem(IdentifierItem("my-append"), IdentifierItem("it"), ListItem(IdentifierItem("cdr"), IdentifierItem("lst"))))))
    )), ""))
  }
}
