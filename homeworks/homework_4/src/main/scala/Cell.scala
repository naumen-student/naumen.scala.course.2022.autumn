sealed trait Cell

class EmptyCell extends Cell {
  override def toString: String = "empty"
}
class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}
class StringCell(text: String) extends Cell {
  override def toString: String = text
}
class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell {
  def getRef: Option[Cell] = table.getCell(ix, iy)

  override def toString: String = {
    def toStringRec(referenceCell: ReferenceCell): String = {
      referenceCell.getRef.map {
        case r: ReferenceCell => if (r == this) "cyclic" else toStringRec(r)
        case r                => r.toString
      }.getOrElse("outOfRange")
    }
    toStringRec(this)
  }
}
