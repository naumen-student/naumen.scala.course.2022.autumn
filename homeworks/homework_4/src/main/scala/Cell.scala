trait Cell {}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = s"$number"
}

class StringCell(string: String) extends Cell {
  override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = toString(List.empty)

  private def toString(refCells: List[Cell]): String = {
    table.getCell(ix, iy) match {
      case None => "outOfRange"
      case Some(cell: ReferenceCell) =>
        if (refCells.contains(cell))
          "cyclic"
        else
          cell.toString(cell :: refCells)
      case Some(cell) => cell.toString
    }
  }
}