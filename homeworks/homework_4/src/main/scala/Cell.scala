trait Cell {
    def toString: String
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
    var data: Option[Cell] = table.getCell(ix, iy)

    override def toString: String = {
        data = table.getCell(ix, iy)
        data match {
            case None => "outOfRange"
            case Some(x) =>
                x match {
                    case cell: ReferenceCell =>
                        cell.data match {
                            case None => "outOfRange"
                            case Some(b) => if (b != this) x.toString else "cyclic"
                        }
                    case _ => x.toString
                }
        }
    }
}