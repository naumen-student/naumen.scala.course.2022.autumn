import scala.annotation.tailrec

trait Cell {
  override final def toString: String = str
  def str: String
}

class EmptyCell extends Cell {
  override def str: String = "empty"
}

class NumberCell(value: Int) extends Cell {
  override def str: String = value.toString
}

class StringCell(value: String) extends Cell {
  override def str: String = value
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  def ptrDerefence: Option[Cell] = table.getCell(ix, iy)
  override def str: String = {
    @tailrec
    def strHelper(referenceCell: ReferenceCell): String = {
      referenceCell.ptrDerefence.getOrElse(None) match {
        case c: ReferenceCell => if (c == this) "cyclic" else strHelper(c)
        case r => r.toString
        case None => "outOfRange"
      }
    }

    strHelper(this)
  }
}