package homework_4

class Table(width: Int, height: Int) {
  private val cells: Array[Cell] = Array.fill[Cell](width * height)(new EmptyCell)

  private def inBounds(ix: Int, iy: Int): Option[Boolean] =
    Some(ix > -1 && ix < width && iy > -1 && iy < height)
      .filter(_ == true)

  def getCell(ix: Int, iy: Int): Option[Cell] =
    inBounds(ix, iy)
      .map(_ => cells(ix + iy * width))  // выполнит если inBound вернет true


  def setCell(ix: Int, iy: Int, cell: Cell): Unit =
    inBounds(ix, iy)
      .foreach(_ => cells(ix + iy * width) = cell) // foreach для того, чтобы не возвращать никакое значение
}
