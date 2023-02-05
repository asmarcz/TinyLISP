package asmar.tinylisp

import parser.*
import secd.compiler.CompilationManager
import secd.runtime.Runtime
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
            val runtime = Runtime(mutable.Stack.from(instructions))
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
  }

  test("logical operators") {run =>
    run("(< 1 2) (< 2 1) (< 2 2)") should equal(List(IntItem(1), IntItem(0), IntItem(0)))
    run("(< 1. 2) (< 2 1.) (< 2. 2)") should equal(List(IntItem(1), IntItem(0), IntItem(0)))
    run("(> 1 2) (> 2 1) (> 2 2)") should equal(List(IntItem(0), IntItem(1), IntItem(0)))
    run("(> 1. 2) (> 2 1.) (> 2. 2)") should equal(List(IntItem(0), IntItem(1), IntItem(0)))
    run("(= 1 2) (= 2 1) (= 2 2)") should equal(List(IntItem(0), IntItem(0), IntItem(1)))
    run("(= 1. 2) (= 2. 1) (= 2. 2)") should equal(List(IntItem(0), IntItem(0), IntItem(1)))
  }
}
