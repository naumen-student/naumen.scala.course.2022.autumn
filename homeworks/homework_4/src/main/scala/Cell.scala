sealed trait Cell

class NumberCell(value: Int) extends Cell {
  override def toString: String = value.toString
}
class StringCell(value: String) extends Cell {
  override def toString: String = value
}
class ReferenceCell(row: Int, column: Int, table: Table) extends Cell {
  override def toString: String =
    try table.getCell(row, column).get.toString
    catch {
      case _: StackOverflowError => "cyclic"
    }
}
class EmptyCell extends Cell {
  override def toString: String = "empty"
}
