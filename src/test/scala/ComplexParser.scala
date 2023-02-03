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

  test("my-append") { parser =>
    parser(
      """(define (my-append it lst)
        |  (if (null? lst)
        |    (cons it nil)
        |    (let (left) ((car lst))
        |      (cons left (my-append it (cdr lst))))))
        |
        |(my-append 3 '(1 2))""".stripMargin
    ) should equal(Accept(List(
      ListItem(
        IdentifierItem("define"), ListItem(IdentifierItem("my-append"), IdentifierItem("it"), IdentifierItem("lst")),
        ListItem(IdentifierItem("if"), ListItem(IdentifierItem("null?"), IdentifierItem("lst")),
          ListItem(IdentifierItem("cons"), IdentifierItem("it"), IdentifierItem("nil")),
          ListItem(IdentifierItem("let"), ListItem(IdentifierItem("left")),
            ListItem(ListItem(IdentifierItem("car"), IdentifierItem("lst"))),
            ListItem(IdentifierItem("cons"), IdentifierItem("left"),
              ListItem(IdentifierItem("my-append"), IdentifierItem("it"), ListItem(IdentifierItem("cdr"), IdentifierItem("lst"))))))),
      ListItem(IdentifierItem("my-append"), IntItem(3), QuotedItem(ListItem(IntItem(1), IntItem(2))))
    ), ""))
  }

  test("fact") { parser =>
    parser(
      """(define (fact n)
        |  (if (= n 0)
        |    1
        |    (* n (fact (- n 1)))))""".stripMargin
    ) should equal(Accept(List(ListItem(
      IdentifierItem("define"), ListItem(IdentifierItem("fact"), IdentifierItem("n")),
      ListItem(IdentifierItem("if"), ListItem(IdentifierItem("="), IdentifierItem("n"), IntItem(0)),
        IntItem(1),
        ListItem(IdentifierItem("*"), IdentifierItem("n"),
          ListItem(IdentifierItem("fact"),
            ListItem(IdentifierItem("-"), IdentifierItem("n"), IntItem(1)))))
    )), ""))

    parser(
      """(let
        |  (fact) ((lambda (n)
        |    (if (= n 0)
        |      1
        |      (* n (fact (- n 1))))))
        |  (fact 5))""".stripMargin
    ) should equal(Accept(List(ListItem(
      IdentifierItem("let"),
      ListItem(IdentifierItem("fact")), ListItem(ListItem(IdentifierItem("lambda"), ListItem(IdentifierItem("n")),
        ListItem(IdentifierItem("if"), ListItem(IdentifierItem("="), IdentifierItem("n"), IntItem(0)),
          IntItem(1),
          ListItem(IdentifierItem("*"), IdentifierItem("n"),
            ListItem(IdentifierItem("fact"), ListItem(IdentifierItem("-"), IdentifierItem("n"), IntItem(1))))))),
      ListItem(IdentifierItem("fact"), IntItem(5))
    )), ""))
  }
}
