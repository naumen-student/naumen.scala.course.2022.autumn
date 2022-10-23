import scala.collection.mutable

class Table(width: Int, height: Int){
  private val table: mutable.Map[(Int, Int), Cell] = mutable.Map.empty

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix > width || iy > height || ix < 0 || iy < 0) None else table.get(ix, iy) match {
      case Some(cell) => Some(cell)
      case None => Option(new EmptyCell)
    }
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = table((ix, iy)) = cell
}