class Table(width: Int, height: Int) {
  private val cells: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (cellInBounds(ix, iy))
      Option(cells(ix + iy * width))
    else None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (cellInBounds(ix, iy))
      cells(ix + iy * width) = cell
  }

  private def cellInBounds(ix: Int, iy: Int): Boolean = ix >= 0 && ix < width && iy >= 0 && iy < height
}