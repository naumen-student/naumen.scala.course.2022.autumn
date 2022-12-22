class Table(width: Int, height: Int) {
  val cells: Array[Array[Cell]] = Array.fill(height) {
    Array.fill(width) { new EmptyCell }
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    try
      Option(cells(ix)(iy))
    catch {
      case _: ArrayIndexOutOfBoundsException => None
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell) : Unit = {
    cells(ix)(iy) = cell
  }
}