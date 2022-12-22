import scala.collection.mutable

class Table(cols: Int, rows: Int) {
  private def isInBounds(col: Int, row: Int): Boolean = col < cols && col >= 0 && row >= 0 && row < rows

  private val inner: mutable.Map[(Int, Int), Cell] = mutable.Map.empty

  def getCell(col: Int, row: Int): Option[Cell] =
    if (isInBounds(col, row)) Some(inner.getOrElse((col, row), EmptyCell())) else None

  def setCell(col: Int, row: Int, cell: Cell): Unit =
    if (isInBounds(col, row)) inner.put((col, row), cell)

}