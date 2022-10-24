trait Cell

case class EmptyCell() extends Cell {
  @Override
  override def toString: String = "empty"
}

case class StringCell(string: String) extends Cell {
  @Override
  override def toString: String = string;
}

case class NumberCell(number : Int) extends Cell {
  @Override
  override def toString: String = number.toString;
}

case class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  @Override
  override def toString: String = {
    tryGetNextCell match {
      case None => "outOfRange"
      case Some(cell : ReferenceCell) =>
        if (cell.tryGetNextCell.contains(this)) "cyclic" else cell.toString
      case Some(cell: Cell) => cell.toString
    }
  }

  private def tryGetNextCell = {
    table.getCell(ix, iy)
  }
}
