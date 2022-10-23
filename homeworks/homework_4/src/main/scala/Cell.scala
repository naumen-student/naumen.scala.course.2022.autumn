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


class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  override def toString: String = {
    try table.getCell(ix, iy).get.toString
    catch {
      case _: StackOverflowError => "cyclic"
    }
  }
}
