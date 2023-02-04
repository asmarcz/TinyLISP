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
  var closureStack: mutable.Stack[Closure] = mutable.Stack(),
  // (stack, closureStack, code, env)
  val dump: mutable.Stack[(mutable.Stack[Item], mutable.Stack[Closure], mutable.Stack[Instruction], Env)] = mutable.Stack(),
  var env: Env = Env(),
  var stack: mutable.Stack[Item] = mutable.Stack(),
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
          case NIL() => stack push NilItem()
          case LDC(constant) => stack push constant
          case LD(coordinates) => ???
          case SEL(b1, b2) =>
            dump push ((null, null, code, null))
            // dump push ((stack.clone(), closureStack.clone(), code, env.clone()))
            code = mutable.Stack.from(if (stack.pop().toBoolean) b1 else b2)
          case JOIN() => code = dump.pop()._3
          case LDF(code) => ???
          case AP() => ???
          case RTN() =>
            val ret = stack.pop()
            val oldState = dump.pop()
            stack = oldState._1
            closureStack = oldState._2
            code = oldState._3
            env = oldState._4
            stack push ret
          case DUM() => ???
          case RAP() => ???
          case DEF() => ???
          case CONS() => stack push ConsItem(stack.pop(), stack.pop())
          case CAR() =>
            val item: Item = stack.pop() match
              case ConsItem(i1, _) => i1
              case ListItem(::(i1, _)) => i1
              case _ => throw RuntimeException("Cons cell or list expected as argument to CAR.")
            stack push item
          case CDR() =>
            val item: Item = stack.pop() match
              case ConsItem(_, i2) => i2
              case ListItem(::(_, lst)) => ListItem(lst)
              case _ => throw RuntimeException("Cons cell or list expected as argument to CDR.")
            stack push item
          // TODO how to generalize
          case ADD() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (IntItem(n1), IntItem(n2)) => IntItem(n1 + n2)
              case (DoubleItem(n1), IntItem(n2)) => DoubleItem(n1 + n2)
              case (IntItem(n1), DoubleItem(n2)) => DoubleItem(n1 + n2)
              case (DoubleItem(n1), DoubleItem(n2)) => DoubleItem(n1 + n2)
              case _ => throw RuntimeException("Non-numerical arguments to ADD.")
            stack push res
          case SUB() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (IntItem(n1), IntItem(n2)) => IntItem(n1 - n2)
              case (DoubleItem(n1), IntItem(n2)) => DoubleItem(n1 - n2)
              case (IntItem(n1), DoubleItem(n2)) => DoubleItem(n1 - n2)
              case (DoubleItem(n1), DoubleItem(n2)) => DoubleItem(n1 - n2)
              case _ => throw RuntimeException("Non-numerical arguments to SUB.")
            stack push res
          case MUL() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (IntItem(n1), IntItem(n2)) => IntItem(n1 * n2)
              case (DoubleItem(n1), IntItem(n2)) => DoubleItem(n1 * n2)
              case (IntItem(n1), DoubleItem(n2)) => DoubleItem(n1 * n2)
              case (DoubleItem(n1), DoubleItem(n2)) => DoubleItem(n1 * n2)
              case _ => throw RuntimeException("Non-numerical arguments to MUL.")
            stack push res
          case DIV() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (IntItem(n1), IntItem(n2)) => IntItem(n1 / n2)
              case (DoubleItem(n1), IntItem(n2)) => DoubleItem(n1 / n2)
              case (IntItem(n1), DoubleItem(n2)) => DoubleItem(n1 / n2)
              case (DoubleItem(n1), DoubleItem(n2)) => DoubleItem(n1 / n2)
              case _ => throw RuntimeException("Non-numerical arguments to DIV.")
            stack push res
          case LT() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (IntItem(n1), IntItem(n2)) => IntItem(b2i(n1 < n2))
              case (DoubleItem(n1), IntItem(n2)) => IntItem(b2i(n1 < n2))
              case (IntItem(n1), DoubleItem(n2)) => IntItem(b2i(n1 < n2))
              case (DoubleItem(n1), DoubleItem(n2)) => IntItem(b2i(n1 < n2))
              case _ => throw RuntimeException("Non-numerical arguments to LT.")
            stack push res
          case GT() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (IntItem(n1), IntItem(n2)) => IntItem(b2i(n1 > n2))
              case (DoubleItem(n1), IntItem(n2)) => IntItem(b2i(n1 > n2))
              case (IntItem(n1), DoubleItem(n2)) => IntItem(b2i(n1 > n2))
              case (DoubleItem(n1), DoubleItem(n2)) => IntItem(b2i(n1 > n2))
              case _ => throw RuntimeException("Non-numerical arguments to GT.")
            stack push res
          case EQ() => stack push IntItem(b2i(stack.pop() == stack.pop()))
      }
    } catch {
      case exception: Exception => exceptionHandler(exception, env)
    }
  }
}
