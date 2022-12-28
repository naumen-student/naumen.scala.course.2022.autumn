package homework_4

sealed trait Cell {
  def toString: String
}

class EmptyCell extends Cell {
  override def toString = "empty"
}

class StringCell(str: String) extends Cell {
  override def toString: String = str
}

class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}

class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
  override def toString: String = toStringImpl()

  private def toStringImpl(cellsHistory: Set[ReferenceCell] = Set.empty): String = {
    table
      .getCell(ix, iy)
      .map({
        case referenceCell: ReferenceCell =>
          Some(cellsHistory.contains(this)) // содержит ли себя
            .filter(_ == false)
            .map(_ => referenceCell.toStringImpl(cellsHistory ++ Set(referenceCell))) // если нет обновить историю
            .getOrElse("cyclic") // иначе циклическая ссылка
        case cell: Cell => cell.toString
      })
      .getOrElse("outOfRange")
  }

}

