package asmar.tinylisp
package secd

import parser.{ConsItem, DoubleItem, IdentifierItem, IntItem, Item, ListItem, QuotedItem}

import scala.annotation.tailrec

sealed trait Instruction

case class NIL() extends Instruction

case class LDC(constant: Item) extends Instruction

case class LD(coordinates: (Int, Int)) extends Instruction {
  override def toString: String = s"LD[${coordinates._1}, ${coordinates._2}]"
}

object LD {
  def apply(_1: Int, _2: Int): LD = LD((_1, _2))
}

case class SEL(b1: List[Instruction], b2: List[Instruction]) extends Instruction

case class JOIN() extends Instruction

case class LDF(code: List[Instruction]) extends Instruction {
  override def toString: String = s"LDF{${code.mkString(", ")}}"
}

object LDF {
  def apply(instructions: Instruction*): LDF = LDF(instructions.toList)
}

case class AP() extends Instruction

case class RTN() extends Instruction

case class DUM() extends Instruction

case class RAP() extends Instruction

case class DEF() extends Instruction

case class CONS() extends Instruction

case class CAR() extends Instruction

case class CDR() extends Instruction

case class ADD() extends Instruction

case class SUB() extends Instruction

case class MUL() extends Instruction

case class DIV() extends Instruction

case class EQ() extends Instruction

case class LT() extends Instruction

case class GT() extends Instruction

class Code(var instructions: List[Instruction], val parent: Option[Code]) {
  def insert(instruction: Instruction): Unit =
    instructions = instructions :+ instruction
}

class Env(var values: Map[String, Int], val parent: Option[Env]) {
  def insert(key: String): Unit =
    values = values + Tuple2(key, values.size)

  @tailrec
  private def coordinates(key: String, depth: Int): (Int, Int) = {
    if (values.contains(key)) (depth, values(key))
    else if (parent.nonEmpty) parent.get.coordinates(key, depth + 1)
    else throw RuntimeException(s"Unknown identifier '$key'.")
  }

  def coordinates(key: String): (Int, Int) =
    coordinates(key, 0)
}

val binaryOperators = List('+', '-', '*', '/', '<', '>', '=')
val unaryOperators = List("car", "cdr")

class CompilationManager {
  private var code: Code = Code(List(), None)
  private var env: Env = Env(Map(), None)

  def compile(items: List[Item]): List[Instruction] = {
    for (item <- items) {
      compile(item)
    }
    code.instructions
  }

  def compile(item: Item): Unit = {
    item match {
      case c: ConsItem => compileCons(c)
      case d: DoubleItem => compileNumber(d)
      case i: IdentifierItem => compileIdentifier(i.value)
      case i: IntItem => compileNumber(i)
      case l: ListItem => compileList(l)
      case q: QuotedItem => compileQuote(q.value)
    }
  }

  private def compileCons(item: ConsItem): Unit = {
    compileBinary(ListItem(IdentifierItem("cons"), item.value._1, item.value._2))
  }

  private def compileQuote(item: Item): Unit = {
    code.insert(LDC(item))
  }

  private def compileIdentifier(name: String): Unit = {
    name match {
      case "nil" => code.insert(NIL())
      case "t" => code.insert(LDC(IntItem(1)))
      case _ => code.insert(LD(env.coordinates(name)))
    }
  }

  private def compileNumber(item: DoubleItem | IntItem): Unit = {
    code.insert(LDC(item))
  }

  private def compileList(item: ListItem): Unit = {
    val lst = item.value
    lst match {
      case ::(IdentifierItem("define"), _) => compileDefine(item)
      case ::(IdentifierItem("if"), _) => compileIf(item)
      case ::(IdentifierItem("let"), ::(names: ListItem, ::(values: ListItem, ::(body: ListItem, Nil)))) =>
        compileLet(names, values, body)
      case ::(IdentifierItem("cons"), ::(it1: Item, ::(it2: Item, Nil))) => compileCons(ConsItem(it1, it2))
      case ::(IdentifierItem(unaryOp), ::(arg: Item, Nil)) if unaryOperators.contains(unaryOp) => compileUnary(unaryOp, arg)
      case ::(IdentifierItem("lambda"), ::(args: ListItem, ::(body: ListItem, Nil)))
        if args.value.forall(_.isInstanceOf[IdentifierItem]) =>
        compileLambda(args.value.asInstanceOf[List[IdentifierItem]], body)
      case ::(IdentifierItem(binOp), _) if binOp.length == 1 && binaryOperators.contains(binOp.head) && lst.length == 3 =>
        compileBinary(item)
      case ::(_: IdentifierItem, _) => compileCall(item)
      case _ => {
        Console.err.println(s"Expected identifier at the start of the list, got $lst instead.")
        throw IllegalArgumentException()
      }
    }
  }

  private def compileLet(names: ListItem, values: ListItem, body: ListItem): Unit = {
    val lst = values.value
    code.insert(NIL())
    lst.reverse.foreach(it => {
      compile(it)
      code.insert(CONS())
    })
    compileLambda(names.value.asInstanceOf[List[IdentifierItem]], body)
    code.insert(AP())
  }

  private def compileUnary(unaryOp: String, arg: Item): Unit = {
    compile(arg)
    code.insert(unaryOp match {
      case "car" => CAR()
      case "cdr" => CDR()
      case _ => throw RuntimeException()
    })
  }

  private def compileCall(item: ListItem): Unit = {
    val lst = item.value
    code.insert(NIL())
    lst.tail.reverse.foreach(it => {
      compile(it)
      code.insert(CONS())
    })
    compile(lst.head)
    code.insert(AP())
  }

  private def defineError(): Nothing = {
    Console.err.println("Expected argument list and function body after define.")
    throw IllegalArgumentException()
  }

  private def compileDefine(item: ListItem): Unit = {
    val lst = item.value
    if (lst.length < 3) defineError()
    val args = lst(1)
    args match {
      case l: ListItem => {
        l.value.head match {
          case IdentifierItem(funName) => {
            env.insert(funName)
            compileLambda(l.value.tail.map({
              case i: IdentifierItem => i
              case _ => defineError()
            }), lst(2).asInstanceOf[ListItem])
            code.insert(DEF())
          }
          case _ => defineError()
        }
      }
      case _ => defineError()
    }
  }

  private def compileBinary(item: ListItem): Unit = {
    item.value match
      case ::(IdentifierItem(binOp), ::(lhs, ::(rhs, Nil))) => {
        compile(rhs)
        compile(lhs)
        binOp match {
          case "+" => code.insert(ADD())
          case "-" => code.insert(SUB())
          case "*" => code.insert(MUL())
          case "/" => code.insert(DIV())
          case "<" => code.insert(LT())
          case ">" => code.insert(GT())
          case "=" => code.insert(EQ())
          case "cons" => code.insert(CONS())
        }
      }
  }

  private def compileIf(item: ListItem): Unit = {
    val lst = item.value
    if (lst.length != 4) {
      Console.err.println("Expected three arguments after if.")
      throw IllegalArgumentException()
    }
    compile(item.value(1))
    newCode()
    compile(item.value(2))
    code.insert(JOIN())
    newCode()
    compile(item.value(3))
    code.insert(JOIN())
    val b2 = oldCode()
    val b1 = oldCode()
    code.insert(SEL(b1, b2))
  }

  private def compileLambda(args: List[IdentifierItem], body: ListItem): Unit = {
    newCode()
    env = Env(Map(), Some(env))
    args.foreach(arg => env.insert(arg.value))
    compile(body)
    code.insert(RTN())
    // Why does this use wrong instance?
    //  code.insert(LDF(oldCode()))
    val ldf = LDF(oldCode())
    code.insert(ldf)
    env = env.parent.get
  }

  private def newCode(): Unit = {
    code = Code(List(), Some(code))
  }

  private def oldCode(): List[Instruction] = {
    val ret = code
    code = code.parent.get
    ret.instructions
  }
}
