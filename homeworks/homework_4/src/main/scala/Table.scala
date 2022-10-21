
class Table(width: Int, height: Int) {
  var dataTable: Array[Cell] = Array.fill(width * height)(new EmptyCell)

  def checkCellIndexes(ix: Int, iy: Int): Boolean = {
    ix < 0 || ix > height || iy < 0 || iy > width
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (checkCellIndexes(ix, iy))
      None
    else
      Some(dataTable(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (!checkCellIndexes(ix, iy))
      dataTable(ix + iy * width) = cell
  }
}