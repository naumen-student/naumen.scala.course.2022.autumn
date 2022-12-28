import scala.collection.mutable.ListBuffer

class Table(width: Int, height: Int) {
  val cells : ListBuffer[Cell] = ListBuffer.fill[Cell](width * height){ new EmptyCell }

  def getCell(x: Int, y: Int)  = {
    if (x >= 0 && x < width && y >= 0 && y < height) {
      Some(cells(y * width + x))
    }
    else None
  }

  def setCell(x: Int, y: Int, cell: Cell): Unit = {
    cells(x + y * width) = cell
  }
}