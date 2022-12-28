case class Table(width: Int, height: Int) {
    val cells: Array[Cell] = Array.fill(width * height) {
        new EmptyCell
    }
    val ind: (Int, Int) => Int = (ix: Int, iy: Int) => ix + iy * width

    def getCell(ix: Int, iy: Int): Option[Cell] = {
        val i = ind(ix, iy)
        if (i > height * width || i < 0)
            None
        else
            Some(cells(i))
    }

    def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
        cells(ind(ix, iy)) = cell
    }
}