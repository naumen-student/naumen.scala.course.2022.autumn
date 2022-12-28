class Table(i: Int, i1: Int) {
  private var array = new Array[Cell](i*i1)
  for (j <- 0 until i*i1 ){
    array(j) = new EmptyCell
  }

  def getCell(i: Int, i1: Int): Option[Cell] = {
    if(i >= 0 && i < this.i && i1 >= 0 && i1 < this.i1){
      return Option(array(i + i1 * this.i1))
    }
    None
  }

  def setCell(ix: Int, iy: Int, cell: Cell) = {
    if(ix >= 0 && ix < i && iy >= 0 && iy < i1){
      array(ix + iy * i1) = cell
    }
  }
}