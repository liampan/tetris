
case class Board(blocks :List[Tetromino], nextTet : List[Tetromino]= Spawner.spawn, tick: Int = 0, score: Int = 0) {

  def tickOne= {
    this.copy(tick = this.tick+1)
  }

  def print = {
    Printer(this.blocks, 10, this.nextTet, this.score)
  }
}