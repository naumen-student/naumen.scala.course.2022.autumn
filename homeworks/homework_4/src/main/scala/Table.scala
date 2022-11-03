import scala.collection.mutable


class Table(weight: Int, height: Int) {
  private val matrix = new mutable.MutableList[mutable.MutableList[Cell]]
  for (_ <- 0 until height) {
    val row = new mutable.MutableList[Cell]()
    for (_ <- 0 until weight) {
      row += new EmptyCell()
    }
    matrix += row
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (isValid(ix, iy))
      Option(matrix(ix)(iy))
    else
      None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (isValid(ix, iy))
      matrix(ix)(iy) = cell
  }
  private def isValid(ix: Int, iy: Int): Boolean = {
    0 <= ix && ix < weight && 0 <= iy && iy < height
  }
}

