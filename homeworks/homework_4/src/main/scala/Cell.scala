trait Cell {
  def toString(): String
}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class StringCell(string: String) extends Cell {
  override def toString: String = string
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  var dataCell = table.getCell(ix, iy)
  override def toString: String = {
    dataCell = table.getCell(ix, iy)
    dataCell match {
      case None => "outOfRange"
      case Some(a) =>
        a match {
          case cell: ReferenceCell =>
            cell.dataCell match {
              case None    => "outOfRange"
              case Some(b) => if (b != this) a.toString else "cyclic"
            }
          case _ => a.toString
        }
    }
  }
}
