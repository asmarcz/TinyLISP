package asmar.tinylisp

import parser.*
import secd.compiler.CompilationManager
import secd.runtime.Runtime

import scala.collection.mutable
import scala.io.Source

@main
def main(filename: String): Unit = {
  val source = Source.fromFile(filename)
  val code = source.mkString
  source.close()

  rep(whitespace(item()))(code) match
    case Accept(value, rem) =>
      if (rem.forall(_.isWhitespace)) {
        val instructions = CompilationManager().compile(value)
        val runtime = Runtime(
          mutable.Stack.from(instructions),
          exceptionHandler = (ex: Exception, _) => throw ex
        )
        runtime.run()
        runtime.getResult.foreach(println)
      }
      else throw IllegalArgumentException(s"Unexpected input at the end: '$rem'")
    case Reject(_) => throw IllegalArgumentException()
}
