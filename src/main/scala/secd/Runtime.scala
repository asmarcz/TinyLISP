package asmar.tinylisp
package secd

import parser.*

import java.lang.Double
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.*

type Element = Either[Closure, Item]

class Env(private val values: ListBuffer[Element] = ListBuffer.empty, val parent: Option[Env] = None) {
  @tailrec
  final def get(index: Int, depth: Int = 0): Element = {
    if (depth == 0) values(index)
    else parent match
      case Some(p) => p.get(index, depth - 1)
      case None => throw InternalError("Trying to access a non-occupied coordinate.")
  }

  def get(coordinates: (Int, Int)): Element = get(coordinates._2, coordinates._1)

  def insert(el: Element): Int = {
    values += el
    values.size - 1
  }

  def _getValues: ListBuffer[Element] = values

  override def clone(): Env = Env(values.clone(), parent.map(_.clone()))

  override def toString: String = {
    parent match
      case Some(p) => String.join("\n", values.toString, p.toString)
      case None => values.toString
  }
}

case class Closure(code: mutable.Stack[Instruction], env: Env)

class Runtime(
  var code: mutable.Stack[Instruction] = mutable.Stack(),
  // (stack, code, env)
  val dump: mutable.Stack[(mutable.Stack[Element], mutable.Stack[Instruction], Env)] = mutable.Stack(),
  var env: Env = Env(),
  var stack: mutable.Stack[Element] = mutable.Stack(),
  val exceptionHandler: (Exception, Env) => Unit = (exception, env) => {
    println(exception)
    println(env)
  }
) {
  extension (item: Item)
    private def toBoolean: Boolean = {
      item match
        case ConsItem((l, r)) => l != NilItem() || r != NilItem()
        case DoubleItem(value) => !(value == 0.0 || value == Double.NaN)
        case IdentifierItem(_) => true
        case IntItem(value) => value != 0
        case ListItem(value) => value.nonEmpty
        case NilItem() => false
        case QuotedItem(value) => value.toBoolean
    }

  private def b2i(b: Boolean): Int = if (b) 1 else 0

  def run(): Unit = {
    try {
      while (code.nonEmpty) {
        code.pop() match
          case NIL() => stack push Right(NilItem())
          case LDC(constant) => stack push Right(constant)
          case LD(coordinates) => ???
          case SEL(b1, b2) =>
            stack.pop() match
              case Left(_) => throw InternalError("Closure on top of stack when deciding SEL branch.")
              case Right(value) =>
                dump push ((null, code, null))
                // dump push ((stack.clone(), code, env.clone()))
                code = mutable.Stack.from(if (value.toBoolean) b1 else b2)
          case JOIN() => code = dump.pop()._2
          case LDF(code) => ???
          case AP() => ???
          case RTN() =>
            val ret = stack.pop()
            val oldState = dump.pop()
            stack = oldState._1
            code = oldState._2
            env = oldState._3
            stack push ret
          case DUM() => ???
          case RAP() => ???
          case DEF() => ???
          case CONS() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(i1), Right(i2)) => ConsItem(i1, i2)
              case _ => throw RuntimeException("Closure argument to CONS.")
            stack push Right(res)
          case CAR() => ???
          case CDR() => ???
          // TODO how to generalize
          case ADD() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(IntItem(n1)), Right(IntItem(n2))) => IntItem(n1 + n2)
              case (Right(DoubleItem(n1)), Right(IntItem(n2))) => DoubleItem(n1 + n2)
              case (Right(IntItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 + n2)
              case (Right(DoubleItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 + n2)
              case _ => throw RuntimeException("Non-numerical arguments to ADD.")
            stack push Right(res)
          case SUB() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(IntItem(n1)), Right(IntItem(n2))) => IntItem(n1 - n2)
              case (Right(DoubleItem(n1)), Right(IntItem(n2))) => DoubleItem(n1 - n2)
              case (Right(IntItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 - n2)
              case (Right(DoubleItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 - n2)
              case _ => throw RuntimeException("Non-numerical arguments to SUB.")
            stack push Right(res)
          case MUL() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(IntItem(n1)), Right(IntItem(n2))) => IntItem(n1 * n2)
              case (Right(DoubleItem(n1)), Right(IntItem(n2))) => DoubleItem(n1 * n2)
              case (Right(IntItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 * n2)
              case (Right(DoubleItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 * n2)
              case _ => throw RuntimeException("Non-numerical arguments to MUL.")
            stack push Right(res)
          case DIV() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(IntItem(n1)), Right(IntItem(n2))) => IntItem(n1 / n2)
              case (Right(DoubleItem(n1)), Right(IntItem(n2))) => DoubleItem(n1 / n2)
              case (Right(IntItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 / n2)
              case (Right(DoubleItem(n1)), Right(DoubleItem(n2))) => DoubleItem(n1 / n2)
              case _ => throw RuntimeException("Non-numerical arguments to DIV.")
            stack push Right(res)
          case LT() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(IntItem(n1)), Right(IntItem(n2))) => IntItem(b2i(n1 < n2))
              case (Right(DoubleItem(n1)), Right(IntItem(n2))) => IntItem(b2i(n1 < n2))
              case (Right(IntItem(n1)), Right(DoubleItem(n2))) => IntItem(b2i(n1 < n2))
              case (Right(DoubleItem(n1)), Right(DoubleItem(n2))) => IntItem(b2i(n1 < n2))
              case _ => throw RuntimeException("Non-numerical arguments to LT.")
            stack push Right(res)
          case GT() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(IntItem(n1)), Right(IntItem(n2))) => IntItem(b2i(n1 > n2))
              case (Right(DoubleItem(n1)), Right(IntItem(n2))) => IntItem(b2i(n1 > n2))
              case (Right(IntItem(n1)), Right(DoubleItem(n2))) => IntItem(b2i(n1 > n2))
              case (Right(DoubleItem(n1)), Right(DoubleItem(n2))) => IntItem(b2i(n1 > n2))
              case _ => throw RuntimeException("Non-numerical arguments to GT.")
            stack push Right(res)
          case EQ() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (Right(i1), Right(i2)) => IntItem(b2i(i1 == i2))
              case _ => throw RuntimeException("Closure argument to EQ.")
            stack push Right(res)
      }
    } catch {
      case exception: Exception => exceptionHandler(exception, env)
    }
  }
}
