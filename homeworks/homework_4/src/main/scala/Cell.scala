trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}

class StringCell(value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  var cell = table.getCell(ix, iy)
  override def toString: String = {
    cell = table.getCell(ix, iy)
    cell match {
      case None => "outOfRange"
      case Some(a) =>
        a match {
          case cell: ReferenceCell =>
            cell.cell match {
              case None => "OutOfRange"
              case Some(b) => if (b != this) a.toString else "cyclic"
            }
          case _ => a.toString
        }
    }
  }
}