import scala.annotation.tailrec

trait Cell
class EmptyCell extends Cell {
  override def toString: String = "empty"
}
class NumberCell(number: Int) extends Cell {
  override def toString: String = number.toString
}
class StringCell(string: String) extends Cell {
  override def toString: String = string
}
class ReferenceCell(val ix: Int, val iy: Int, val table: Table) extends Cell {
  @tailrec
  private def go(x: Int, y: Int, next: ReferenceCell): String = {
    next.table.getCell(x, y) match {
      case Some(value) =>
        value match {
          case referenceCell: ReferenceCell if referenceCell.ix == ix && referenceCell.iy == iy => "cyclic"
          case referenceCell: ReferenceCell                                                     => go(referenceCell.ix, referenceCell.iy, referenceCell)
          case cell                                                                             => cell.toString
        }
      case None => "outOfRange"
    }
  }

  override def toString: String = go(ix, iy, this)
}
