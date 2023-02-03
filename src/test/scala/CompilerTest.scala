package asmar.tinylisp

import parser.*
import secd.*

import org.scalatest.Outcome
import org.scalatest.funsuite.FixtureAnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.matchers.*

class CompilerTest extends FixtureAnyFunSuite {
  override protected type FixtureParam = String => List[Instruction]

  override protected def withFixture(test: OneArgTest): Outcome = {
    val codeToInstructions = (code: String) => {
      rep(whitespace(item()))(code) match
        case Accept(value, _) => CompilationManager().compile(value)
        case Reject(_) => throw IllegalArgumentException()
    }
    try test(codeToInstructions)
    finally {}
  }

  test("define null?") { compile =>
    compile(
      """(define (null? x)
        |  (= x nil))""".stripMargin
    ) should equal(List(LDF(NIL(), LD(0, 0), EQ(), RTN()), DEF()))
  }

  test("infinite rec") { compile =>
    compile(
      """(define (rec x)
        |  (rec (+ x 1)))""".stripMargin
    ) should equal(List(LDF(
      NIL(), LDC(IntItem(1)), LD(0, 0), ADD(), CONS(), LD(1, 0), AP(), RTN()
    ), DEF()))
  }

  test("branching") { compile =>
    compile("(if (= 0 1) 2 3)") should equal(List(
      LDC(IntItem(1)),
      LDC(IntItem(0)),
      EQ(),
      SEL(
        List(LDC(IntItem(2)), JOIN()),
        List(LDC(IntItem(3)), JOIN())
      )
    ))
  }

  test("unknown identifier") { compile =>
    the[RuntimeException] thrownBy {
      compile("(foo 2)")
    } should have message "Unknown identifier 'foo'."
  }

  test("let") { compile =>
    compile(
      """(define (func arg1 arg2)
        |  (let (var1 var2) (arg1 arg2)
        |    (+ var1 var2)))
        |""".stripMargin
    ) should equal(List(
      LDF(
        NIL(),
        LD(0, 1),
        CONS(),
        LD(0, 0),
        CONS(),
        LDF(
          LD(0, 1),
          LD(0, 0),
          ADD(),
          RTN()
        ),
        AP(),
        RTN()
      ),
      DEF()
    ))
  }

  test("lambda") { compile =>
    compile(
      """(define (foo lamb)
        |  (lamb 2))
        |
        |(foo (lambda (n) (+ n 3)))""".stripMargin
    ) should equal(List(
      LDF(
        NIL(),
        LDC(IntItem(2)),
        CONS(),
        LD(0, 0),
        AP(),
        RTN()
      ),
      DEF(),
      NIL(),
      LDF(
        LDC(IntItem(3)),
        LD(0, 0),
        ADD(),
        RTN()
      ),
      CONS(),
      LD(0, 0),
      AP()
    ))
  }

  test("my-append") { compile =>
    compile(
      """(define (null? x)
        |  (= x nil))
        |
        |(define (my-append it lst)
        |  (if (null? lst)
        |    (cons it nil)
        |    (let (left) ((car lst))
        |      (cons left (my-append it (cdr lst))))))
        |
        |(my-append 3 '(1 2))""".stripMargin
    ) should equal(List(
      LDF(NIL(), LD(0, 0), EQ(), RTN()), DEF(),
      LDF(
        NIL(), LD(0, 1), CONS(), LD(1, 0), AP(), SEL(
          List(NIL(), LD(0, 0), CONS(), JOIN()),
          List(NIL(), LD(0, 1), CAR(), CONS(),
            LDF(
              NIL(), LD(1, 1), CDR(), CONS(), LD(1, 0), CONS(), LD(2, 1), AP(),
              LD(0, 0), CONS(),
              RTN()
            ),
            AP(),
            JOIN()
          )
        ),
        RTN()
      ),
      DEF(),
      NIL(), LDC(ListItem(IntItem(1), IntItem(2))), CONS(),
      LDC(IntItem(3)), CONS(),
      LD(0, 1), AP()
    ))
  }

  test("fact") { compile =>
    the[RuntimeException] thrownBy compile(
      """(let
        |  (fact) ((lambda (n)
        |    (if (= n 0)
        |      1
        |      (* n (fact (- n 1))))))
        |  (fact 5))""".stripMargin
    ) should have message "Unknown identifier 'fact'."
  }
}
