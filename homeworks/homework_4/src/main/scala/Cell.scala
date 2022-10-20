import scala.annotation.tailrec

trait Cell{
  def toString(): String
}

class EmptyCell extends Cell{
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell{
  override def toString: String = number.toString
}

class StringCell(string: String) extends Cell{
  override def toString: String = string
}

class ReferenceCell(val ix: Int, val iy: Int, table: Table) extends Cell{
  def getReferenceCell(): String = {
    @tailrec
    def getCycleCell(currCell: Cell): String = {
      currCell match {
        case refCell: ReferenceCell => if (refCell.ix == ix && refCell.iy == iy) "cyclic" else getCycleCell(table.getCell(refCell.ix, refCell.iy).get)
        case default => default.toString
      }
    }
    getCycleCell(table.getCell(ix, iy).get)
  }
  override def toString: String = {
    if(table.getCell(ix, iy).isEmpty)
      return "outOfRange"
    getReferenceCell()
  }
}