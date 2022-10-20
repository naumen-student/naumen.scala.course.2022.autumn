
case class Table(length: Int, width: Int) {
  var content: Array[Cell] = Array.fill(width * length)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = if (checkIndex(ix, iy)) Some(content(length * iy + ix)) else None

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (checkIndex(ix, iy)) {
      content(length * iy + ix) = cell
    }
  }

  def checkIndex(ix: Int, iy: Int): Boolean = {
    0 <= ix && ix <= length && 0 <= iy && iy <= width
  }
}