import scala.collection.mutable

class Table (width: Int, height: Int) {
  private val cells = mutable.Map[(Int, Int), Cell]()

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= 0 && ix < height && iy >= 0 && iy < width) {
      cells.get(ix, iy) match {
        case Some(cell) => Some(cell)
        case None => Option(new EmptyCell)
      }
    } else {
      None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = cells((ix, iy)) = cell
}