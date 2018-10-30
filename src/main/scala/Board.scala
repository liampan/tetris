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

  def gameOverCheck: Board ={
    if (this.blocks.exists(t => t.getIndex/10 == 0 && !t.userCanControl && !t.canFall )){
      this.tickFive.print
      println(Console.BLINK+ Console.RED +"       GAME OVER" + Console.RESET)

      System.exit(0)
      this
    }
    else this
  }

  def gravity : Board = {
    val nb = if (this.tick%5 == 0){blocks.map(t => t.fall(10))} else this.blocks
    this.copy(blocks = nb)
  }

  def moveBlocks(userInput: Char): Board = {
    val blocks = this.blocks
    val userControlledTets = blocks.filter(_.userCanControl)
    val staticTets = blocks.filterNot(_.userCanControl)
    val newBlocks = userInput match {
      case 'a' => staticTets ++ userControlledTets.map(tetromino => tetromino.moveLeft(blocks))
      case 'd' => staticTets ++ userControlledTets.map(tetromino => tetromino.moveRight(blocks))
      case 'w' => Rotate.rotate(blocks)
      case _   => blocks
    }
    this.copy(blocks = newBlocks)
  }

  def collision: Board ={
    val blocks = this.blocks

    val movingTets = blocks.filter(t =>  t.canFall)
    val playerTets = blocks.filter(t =>  t.userCanControl)
    val uncontrolledTetsIndexes = blocks.filterNot(_.userCanControl).map(_.getIndex)
    val nonMovingTetsIndexes = blocks.filterNot(t =>  t.canFall).map(_.getIndex).toSet
    val movingTetsFutureIndexes = blocks.filter(t =>  t.canFall).map(_.getIndex+10).toSet


    val a = blocks.map(t => if(t.getIndex+10>149) t.freeze else t)
    //this block has hit the bottom so freezes

    val b = if (nonMovingTetsIndexes.intersect(movingTetsFutureIndexes).nonEmpty){a.map(_.freeze)} else a
    //a moving block has hit a frozen block

    val c = if (playerTets.exists(t => (t.getIndex+1)%10 == 0)){b.map(t => t.copy(canMoveRight = false))}else b
    //something has hit the right wall, nothing can move right

    val d = if (playerTets.exists(t => (t.getIndex-1)%10 == 9 || (t.getIndex<0) && (t.getIndex-1)%10 == -1 || t.getIndex == 0))
    {c.map(_.copy(canMoveLeft = false))} else c
    //something has hit the left wall, nothing can move left

    val e = if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex-1))){d.map(_.copy(canMoveLeft = false))} else d
    //something has hit a block to its left

    val f = if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.getIndex+1))){e.map(_.copy(canMoveRight = false))} else e
    //something has hit a block to its right
    this.copy(blocks = f)
  }

}