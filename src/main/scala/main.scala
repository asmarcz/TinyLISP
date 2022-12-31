package asmar.tinylisp

import parser.*
import secd.*

val testInput1 =
  """(define (null? x)
    |  (= x nil))
    |
    |(define (my-append it lst)
    |  (if (null? lst)
    |    (cons it nil)
    |    (let (left) ((car lst))
    |      (cons left (my-append it (cdr lst))))))
    |""".stripMargin

val testInput2 =
  """(define (rec x)
    |  (rec (+ x 1)))
    |""".stripMargin

val tests = List(
  (
    testInput1,
    "List(ListItem(List(IdentifierItem(define), ListItem(List(IdentifierItem(null?), IdentifierItem(x))), ListItem(List(IdentifierItem(=), IdentifierItem(x), IdentifierItem(nil))))), ListItem(List(IdentifierItem(define), ListItem(List(IdentifierItem(my-append), IdentifierItem(it), IdentifierItem(lst))), ListItem(List(IdentifierItem(if), ListItem(List(IdentifierItem(null?), IdentifierItem(lst))), ListItem(List(IdentifierItem(cons), IdentifierItem(it), IdentifierItem(nil))), ListItem(List(IdentifierItem(let), ListItem(List(IdentifierItem(left))), ListItem(List(ListItem(List(IdentifierItem(car), IdentifierItem(lst))))), ListItem(List(IdentifierItem(cons), IdentifierItem(left), ListItem(List(IdentifierItem(my-append), IdentifierItem(it), ListItem(List(IdentifierItem(cdr), IdentifierItem(lst))))))))))))))",
    "List(LDF(List(NIL, LD((0,0)), EQ(), RTN())), DEF(), LDF(List(NIL, LD((0,1)), CONS(), LD((1,0)), AP(), SEL(List(NIL, LD((0,0)), CONS(), JOIN()),List(NIL, LD((0,1)), CAR(), CONS(), LDF(List(NIL, LD((1,1)), CDR(), CONS(), LD((1,0)), CONS(), LD((2,1)), AP(), LD((0,0)), CONS(), RTN())), AP(), JOIN())), RTN())), DEF())"
  ),
  (
    testInput2,
    "List(ListItem(List(IdentifierItem(define), ListItem(List(IdentifierItem(rec), IdentifierItem(x))), ListItem(List(IdentifierItem(rec), ListItem(List(IdentifierItem(+), IdentifierItem(x), IntItem(1))))))))",
    "List(LDF(List(NIL, LDC(IntItem(1)), LD((0,0)), ADD(), CONS(), LD((1,0)), AP(), RTN())), DEF())"
  )
)

@main
def main(): Unit = {
  for (test <- tests) {
    rep(whitespace(item()))(test._1) match {
      case Reject(msg) => Console.err.println(msg)
      case Accept(items, rem) => {
        if (!rem.isBlank) {
          Console.err.println("Unexpected input at the end:")
          Console.err.println(rem)
          throw IllegalArgumentException()
        }
        println(items)
        assert(items.toString == test._2)
        val compilationManager = CompilationManager()
        val instructions = compilationManager.compile(items)
        println(instructions)
        assert(instructions.toString == test._3)
      }
    }
  }
}
