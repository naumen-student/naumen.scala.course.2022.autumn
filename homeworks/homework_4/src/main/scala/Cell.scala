sealed trait Cell

class EmptyCell extends Cell{
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell{
  override def toString: String = number.toString
}

class StringCell(text: String) extends Cell{
  override def toString: String = text
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell{
  def getIX() = ix
  def getIY() = iy

  override def toString: String = getReferenceToString(table.getCell(ix, iy), ix, iy)

  private def getReferenceToString(currentCell: Option[Cell], firstX: Int, firstY: Int): String = currentCell match {
      case None => "outOfRange"
      case Some(cell) => cell match {
        case c@(_: StringCell | _: NumberCell) => c.toString
        case c: ReferenceCell => if (c.getIX == firstX && c.getIY == firstY) "cyclic" else
          getReferenceToString(table.getCell(c.getIX, c.getIY), firstX, firstY)
      }
  }
}