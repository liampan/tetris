case class Board(blocks :List[Tetromino], nextTet : List[Tetromino]= Spawner.spawn, tick: Int = 0, score: Int = 0) {

  def tickOne: Board = {
    this.copy(tick = this.tick+1)
  }

  private def tickFive: Board = {
    this.copy(tick = this.tick+5)
  }

  def print = {
    Printer(this.blocks, 10, this.nextTet, this.score)
  }

  def spawn: Board = {
    val spawnRate = 100
    if (this.tick%spawnRate == 0){
      this.copy(
        blocks = this.blocks ++ this.nextTet,
        nextTet = Spawner.spawn
      )
    }
    else this
  }

  def fullLineCheck: Board = {
    val blocks = this.blocks
    val nonMovingTets = blocks.filterNot(_.userCanControl)
    val lines = nonMovingTets.map(_.getIndex).map(i => i/10)

    val mapped = lines.groupBy(i=>i).mapValues(_.length)
    val fullLines = mapped.mapValues(_ == 10).filter(i => i._2).keys.toList
    val increaseScoreBy = fullLines.length

    val restartedBlocks = if (fullLines.nonEmpty){
      val lowestLine = fullLines.max
      val aboveLine = blocks.filter(t => t.getIndex < lowestLine * 10)
      aboveLine.map(t => t.restartGravity)
    } else Nil

    this.copy(
      blocks = blocks.filterNot(t => fullLines.contains(t.getIndex/10)).filterNot(t => fullLines.nonEmpty && t.getIndex < fullLines.max * 10) ++ restartedBlocks,
      score = this.score + increaseScoreBy)
  }

  def gameOver: Either[Unit, Board] ={
    if (this.blocks.exists(t => t.getIndex/10 == 0 && !t.userCanControl && !t.canFall )){
      this.tickFive.print
      Left(System.exit(0))
    }
    else Right(this)
  }

}