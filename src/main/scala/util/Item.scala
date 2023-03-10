package asmar.tinylisp
package util

trait Item

case class ConsItem(value: (Item, Item)) extends Item

object ConsItem {
  def apply(i1: Item, i2: Item): ConsItem = ConsItem((i1, i2))
}

case class DoubleItem(value: Double) extends Item

case class IdentifierItem(value: String) extends Item

case class IntItem(value: Int) extends Item

case class ListItem(value: List[Item]) extends Item

object ListItem {
  def apply(items: Item*): ListItem = ListItem(items.toList)
}

case class NilItem() extends Item

case class QuotedItem(value: Item) extends Item
