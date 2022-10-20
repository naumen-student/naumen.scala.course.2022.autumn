import scala.collection.mutable.MutableList

class Table(width: Int, height: Int) {
  private val table: MutableList[Cell] = MutableList.fill(width * height)(new EmptyCell)

  private def isInBound(x: Int, y: Int): Boolean =
    x >= 0 && x < width && y >= 0 && y < height

  def getCell(x: Int, y: Int): Option[Cell] =
    Some(isInBound(x, y)).filter(identity).map(_ => table(x + y * height))

  def setCell(x: Int, y: Int, cell: Cell): Unit =
    Some(isInBound(x, y)).filter(identity).map(_ => table.update(x + y * height, cell)).getOrElse(())
}
