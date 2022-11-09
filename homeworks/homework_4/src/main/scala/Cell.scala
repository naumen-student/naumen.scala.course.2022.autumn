import scala.annotation.tailrec

sealed trait Cell {
  override def toString: String = repr()

  def repr(): String
}

case class EmptyCell() extends Cell {
  override def repr(): String = "empty"
}

case class NumberCell(value: Int) extends Cell {
  override def repr(): String = value.toString
}

case class StringCell(value: String) extends Cell {
  override def repr(): String = value
}

case class ReferenceCell(col: Int, row: Int, table: Table) extends Cell {
  self =>
  override def repr(): String = {
    @tailrec
    def goCycle(from: ReferenceCell, visited: Set[ReferenceCell]): String =
      table.getCell(from.col, from.row) match {
        case Some(x: ReferenceCell) if !(visited.contains(x)) => goCycle(x, visited + from)
        case Some(_: ReferenceCell) => "cyclic"
        case Some(otherCell) => otherCell.repr()
        case None => "outOfRange"
      }

    goCycle(self, Set.empty)
  }
}