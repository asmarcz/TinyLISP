package asmar.tinylisp

import parser.*
import secd.compiler.CompilationManager
import secd.runtime.{Env, Runtime}
import util.*

import org.scalatest.Outcome
import org.scalatest.funsuite.FixtureAnyFunSuite
import org.scalatest.matchers.*
import org.scalatest.matchers.should.Matchers.*

import scala.collection.mutable

class RuntimeTest extends FixtureAnyFunSuite {
  override protected type FixtureParam = String => List[Item]

  override protected def withFixture(test: OneArgTest): Outcome = {
    val codeToResult = (code: String) => {
      rep(whitespace(item()))(code) match
        case Accept(value, rem) =>
          if (rem.forall(_.isWhitespace)) {
            val instructions = CompilationManager().compile(value)
            val runtime = Runtime(
              mutable.Stack.from(instructions),
              exceptionHandler = (ex: Exception, _) => throw ex
            )
            runtime.run()
            runtime.getResult
          }
          else throw IllegalArgumentException(s"Unexpected input at the end: '$rem'")
        case Reject(_) => throw IllegalArgumentException()
    }
    try test(codeToResult)
    finally {}
  }

  test("add") { run =>
    run("(+ 1 2)") should equal(List(IntItem(3)))
  }

  test("built-in arithmetic") { run =>
    run("(* (+ 1 2) 5)") should equal(List(IntItem(15)))

    run("(* (+ 1 2) (/ 5. 2))") should equal(List(DoubleItem(7.5)))
    run("(* (+ 1 2) (/ 5 2))") should equal(List(IntItem(6)))

    run("(- 5.7 2)") should equal(List(DoubleItem(3.7)))
  }

  test("logical operators") { run =>
    run("(< 1 2) (< 2 1) (< 2 2)") should equal(List(IntItem(1), IntItem(0), IntItem(0)))
    run("(< 1. 2) (< 2 1.) (< 2. 2)") should equal(List(IntItem(1), IntItem(0), IntItem(0)))
    run("(> 1 2) (> 2 1) (> 2 2)") should equal(List(IntItem(0), IntItem(1), IntItem(0)))
    run("(> 1. 2) (> 2 1.) (> 2. 2)") should equal(List(IntItem(0), IntItem(1), IntItem(0)))
    run("(= 1 2) (= 2 1) (= 2 2)") should equal(List(IntItem(0), IntItem(0), IntItem(1)))
    run("(= 1. 2) (= 2. 1) (= 2. 2)") should equal(List(IntItem(0), IntItem(0), IntItem(1)))
  }

  test("list and cons operations") { run =>
    run("(cons 1 2)") should equal(List(ConsItem(IntItem(1), IntItem(2))))
    run("(1 . 2)") should equal(List(ConsItem(IntItem(1), IntItem(2))))

    run(
      """(define (cadr lst)
        |  (car (cdr lst)))
        |
        |(define (cddr lst)
        |  (cdr (cdr lst)))
        |
        |(car '(1 2))
        |(cdr '(1 2))
        |(cadr '(1 2))
        |
        |(car (1 . 2))
        |(cdr (1 . 2))
        |
        |(car '(1 2 3))
        |(cdr '(1 2 3))
        |(cadr '(1 2 3))
        |
        |(car (1 . (2 . 3)))
        |(cdr '(1 . (2 . 3)))
        |(cadr '(1 . (2 . 3)))
        |
        |(cddr '(1 2))
        |(cddr (1 . (2 . nil)))""".stripMargin
    ) should equal(List(
      IntItem(1), ListItem(IntItem(2)), IntItem(2),
      IntItem(1), IntItem(2),
      IntItem(1), ListItem(IntItem(2), IntItem(3)), IntItem(2),
      IntItem(1), ConsItem(IntItem(2), IntItem(3)), IntItem(2),
      NilItem(),
      NilItem()
    ))

    the[RuntimeException] thrownBy
      run("(cdr (cdr '(1)))") should have message "Cons cell or list expected as argument to CDR."

    the[RuntimeException] thrownBy
      run("(cdr (cdr (1 . nil)))") should have message "Cons cell or list expected as argument to CDR."
  }
}
