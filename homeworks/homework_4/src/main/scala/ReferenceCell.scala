class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  def get_reference_cell: Option[Cell] = table.getCell(ix, iy)

  override def toString: String = {
    val cell_option = get_reference_cell
    if (cell_option.isEmpty) {
      return "outOfRange"
    }
    var cell = cell_option.get
    var used_cells: Set[Cell] = Set(cell)
    while (cell.isInstanceOf[ReferenceCell]) {
      val ref_cell: ReferenceCell = cell.asInstanceOf[ReferenceCell]
      val new_ref_cell_option = ref_cell.get_reference_cell
      if (new_ref_cell_option.isEmpty) {
        return "outOfRange"
      }
      if (used_cells.contains(new_ref_cell_option.get)) {
        return "cyclic"
      }
      used_cells += new_ref_cell_option.get
      cell = new_ref_cell_option.get
    }
    cell.toString
  }
}
