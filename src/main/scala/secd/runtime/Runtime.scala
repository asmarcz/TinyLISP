package asmar.tinylisp
package secd
package runtime

import util.*

import java.lang.Double
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.*

class Env(private val values: ListBuffer[Item] = ListBuffer.empty, val parent: Option[Env] = None) {
  @tailrec
  final def get(index: Int, depth: Int = 0): Item = {
    if (depth == 0)
      if (index >= values.size)
        throw InternalError("Trying to access a non-occupied coordinate.")
      values(index)
    else parent match
      case Some(p) => p.get(index, depth - 1)
      case None => throw InternalError("Trying to access a non-occupied coordinate.")
  }

  def get(coordinates: (Int, Int)): Item = get(coordinates._2, coordinates._1)

  def insert(el: Item): Int = {
    values += el
    values.size - 1
  }

  def _getValues: ListBuffer[Item] = values

  override def clone(): Env = Env(values.clone(), parent.map(_.clone()))

  override def toString: String = {
    parent match
      case Some(p) => String.join("\n", values.toString, p.toString)
      case None => values.toString
  }
}

case class Closure(code: mutable.Stack[Instruction], env: Env) extends Item

class Runtime(
  var code: mutable.Stack[Instruction] = mutable.Stack(),
  // (stack, code, env)
  val dump: mutable.Stack[(mutable.Stack[Item], mutable.Stack[Instruction], Env)] = mutable.Stack(),
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

  private def cons2List(c: ConsItem): List[Item] = {
    c.value match
      case (_, NilItem()) => List(c.value._1)
      case (_, c2: ConsItem) => ::(c.value._1, cons2List(c2))
      case _ => List(c.value._1, c.value._2)
  }

  private def isNil(i: Item): Boolean = {
    i match
      case NilItem() => true
      case ListItem(value) => value.isEmpty
      case _ => false
  }

  def run(): Unit = {
    try {
      while (code.nonEmpty) {
        code.pop() match
          case NIL() => stack push NilItem()
          case LDC(constant) => stack push constant
          case LD(coordinates) => stack push (env get coordinates)
          case SEL(b1, b2) =>
            dump push ((null, code, null))
            // dump push ((stack.clone(), code, env.clone()))
            code = mutable.Stack.from(if (stack.pop().toBoolean) b1 else b2)
          case JOIN() => code = dump.pop()._2
          case LDF(code) => stack push Closure(mutable.Stack.from(code), env.clone())
          case AP() =>
            dump push ((stack, code, env))
            (stack.pop(), stack.pop()) match
              // case p: (Closure, ConsItem) => {} cannot be checked at runtime, why?
              case (closure: Closure, NilItem()) =>
                code = mutable.Stack.from(closure.code)
                env = Env(parent = Some(closure.env))
              case (closure: Closure, args: ConsItem) =>
                code = mutable.Stack.from(closure.code)
                env = Env(ListBuffer.from(cons2List(args)), Some(closure.env))
              case _ => throw RuntimeException("Closure and Cons required on stack before calling AP.")
            stack = mutable.Stack()
          case RTN() =>
            val ret = stack.pop()
            val oldState = dump.pop()
            stack = oldState._1
            code = oldState._2
            env = oldState._3
            stack push ret
          case DUM() => ???
          case RAP() => ???
          case DEF() => stack.pop() match
            case closure: Closure =>
              closure.env.insert(closure)
              env.insert(closure)
            case _ => throw RuntimeException("Closure required on stack before calling DEF.")
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
              case ListItem(::(_, lst)) => lst match
                case Nil => NilItem()
                case _ => ListItem(lst)
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
          case EQ() =>
            val res: Item = (stack.pop(), stack.pop()) match
              case (DoubleItem(n1), IntItem(n2)) => IntItem(b2i(n1 == n2))
              case (IntItem(n1), DoubleItem(n2)) => IntItem(b2i(n1 == n2))
              case (DoubleItem(n1), DoubleItem(n2)) => IntItem(b2i(n1 == n2))
              case (NilItem(), i: Item) => IntItem(b2i(isNil(i)))
              case (i: Item, NilItem()) => IntItem(b2i(isNil(i)))
              case (i1: Item, i2: Item) => IntItem(b2i(i1 == i2))
            stack push res
      }
    } catch {
      case exception: Exception => exceptionHandler(exception, env)
    }
  }

  def getResult: List[Item] = List.from(stack.reverseIterator)
}
