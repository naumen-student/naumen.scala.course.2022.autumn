class Table(width: Int, height: Int) {
  val cells: Array[Cell] = Array.fill(width * height) { new EmptyCell() }

  private def get_index(ix: Int, iy: Int): Int = {
    if (ix >= width || iy >= height || ix < 0 || iy < 0)
      return -1
    ix + iy * this.width
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    val i = get_index(ix, iy)
    if (i == -1)
      return None
    Option(this.cells(i))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    val i = get_index(ix, iy)
    if (i != -1) {
      this.cells(i) = cell
    }
  }
}