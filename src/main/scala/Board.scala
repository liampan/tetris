
case class Board(blocks :List[Tetromino], presentTet : List[Tetromino] = Spawner.spawn, nextTet : List[Tetromino]= Spawner.spawn, tick: Int = 0) {

  def tickOne= {
    this.copy(tick = this.tick+1)
  }
}