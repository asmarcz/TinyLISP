package asmar.tinylisp
package parser

sealed trait Result[+A]
case class Accept[+B](value: B, rem: String) extends Result[B]
case class Reject(msg: String) extends Result[Nothing]
