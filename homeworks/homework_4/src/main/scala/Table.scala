class Table(val h: Int, val w: Int) {
  private val cells = collection.mutable.Map[(Int, Int), Cell]();

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    val cell = cells.get((ix, iy))
    if (cell.isDefined)
      return cell
    if (ix >= 0 && ix < h && iy >= 0 && iy < w) return Some(new EmptyCell())
    return None
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    if (ix < 0 || ix >= h || iy < 0 || iy >= w)
      throw new IndexOutOfBoundsException()
    cells((ix, iy)) = cell
  }
}