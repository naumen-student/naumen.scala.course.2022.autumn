import Table.SetCell

import scala.collection.mutable.ListBuffer

class Table(
             width: Int,
             height: Int) {

  private val diffs = ListBuffer.empty[SetCell]

  def getCell(ix: Int, iy: Int): Option[Cell] =
    if (ix >= 0 && iy >= 0 && ix < width && iy < height) {
      Some(
        diffs.find(
          tc => tc.iy == iy && tc.ix == ix
        ).map(_.cell).getOrElse(new EmptyCell)
      )
    } else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    diffs.append(SetCell(ix, iy, cell))

}

object Table {
  case class SetCell(ix: Int, iy: Int, cell: Cell)
}