trait Cell{
  def toString: String
}

class NumberCell(n: Int) extends Cell {
  override def toString:String = s"$n"
}

class EmptyCell extends Cell{
  override def toString: String = "empty"
}

class StringCell(s: String) extends Cell{
  override def toString: String = s
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {

  override def toString: String = toStringInternal(List.empty)

  private def toStringInternal(refCells: List[Cell]): String = {
    table.getCell(ix, iy) match {
      case None => "outOfRange"
      case Some(cell : ReferenceCell) =>
        if (refCells.contains(cell))
          "cyclic"
        else
          cell.toStringInternal(cell :: refCells)
      case Some(cell) => cell.toString
    }
  }
}