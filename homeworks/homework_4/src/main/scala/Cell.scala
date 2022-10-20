trait Cell {
  var content: Any
  override def toString: String = content.toString
}

class EmptyCell extends Cell{
  override var content: Any = None
  override def toString: String = "empty"
}

class NumberCell(value: Int) extends Cell{
  override var content: Any = value
}

class StringCell(value: String) extends Cell{
  override var content: Any = value
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  override var content: Any = table.getCell(ix, iy)
  override def toString: String = {
    content = table.getCell(ix, iy)
    content match {
      case None => "outOfRange"
      case Some(s) =>
        s match {
          case cell: ReferenceCell =>
            cell.content match {
              case Some(m) => if (m == this) "cyclic" else s.toString
              case None => "outOfRange"
            }
          case _ => s.toString
        }
    }
  }
}