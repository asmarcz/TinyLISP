package asmar.tinylisp
package util

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
