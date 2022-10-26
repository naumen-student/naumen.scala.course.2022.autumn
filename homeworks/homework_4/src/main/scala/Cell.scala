sealed trait Cell

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}

class StringCell(value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
  override def toString: String = table.getCell(ix, iy).map {
    case refCell: ReferenceCell => table
      .getCell(refCell.ix, refCell.iy)
      .filter(_ != this).map(_.toString)
      .getOrElse("cyclic")
    case cell: Cell => cell.toString
  }.getOrElse("outOfRange")
}
