import scala.collection.mutable

class Table(width: Int, height: Int) {
  private val data: mutable.Map[(Int, Int), Cell] = mutable.Map.empty

  def getCell(row: Int, coll: Int): Option[Cell] =
    if ((0 until height).contains(row) && (0 until width).contains(coll))
      Some(data.getOrElse((row, coll), new EmptyCell))
    else None

  def setCell(row: Int, coll: Int, value: Cell): Unit =
    data.put((row, coll), value)
}
