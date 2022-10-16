class Cell;

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}

class StringCell(value: String) extends Cell {
  override def toString: String = value
}

class ReferenceCell(val x: Int, val y: Int, table: Table) extends Cell {

  override def toString: String = {
    if (x < 0 || x >= table.h || y < 0 || y >= table.w) {
      return "outOfRange"
    }
    val visited = collection.mutable.Set[(Int, Int)]()
    var next = (x, y)
    while (!visited.contains(next)) {
      var cell = table.getCell(next._1, next._2).get
      if (cell.isInstanceOf[ReferenceCell]) {
        visited.add(next)
        var rf = cell.asInstanceOf[ReferenceCell]
        next = (rf.x, rf.y)
      }
      else return cell.toString
    }
    "cyclic"
  }
}

