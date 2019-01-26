

case class Board(
                  blocks :List[Tetromino],
                  nextTet : List[Tetromino]= Spawner.spawn,
                  tick: Int = 0,
                  score: Int = 0,
                  name: String,
                  speed: Int = 100
                ) {

  def tickOne: Board = {
    this.copy(tick = this.tick+1)
  }

  private def tickFive: Board = {
    this.copy(tick = this.tick+5)
  }

  def evaluateSpeed: Board = {
    if (this.speed != 10 & tick%1000 == 0) {
      this.copy(speed = speed - 10)
    } else this
  }

  def print = {
    Printer(this.blocks, 10, this.nextTet, this.score, this.name, 10-speed/10)
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
    val lines = nonMovingTets.map(_.index).map(i => i/10)

    val mapped = lines.groupBy(i=>i).mapValues(_.length)
    val fullLines = mapped.mapValues(_ == 10).filter(i => i._2).keys.toList
    val increaseScoreBy = fullLines.length

    val restartedBlocks = if (fullLines.nonEmpty){
      val lowestLine = fullLines.max
      val aboveLine = blocks.filter(t => t.index < lowestLine * 10)
      aboveLine.map(t => t.restartGravity)
    } else Nil

    this.copy(
      blocks = blocks.filterNot(t => fullLines.contains(t.index/10)).filterNot(t => fullLines.nonEmpty && t.index < fullLines.max * 10) ++ restartedBlocks,
      score = this.score + increaseScoreBy)
  }


  def gameOverAction: Board ={
    if (this.blocks.exists(t => t.index/10 == 0 && !t.userCanControl && !t.canFall )){
      this.tickFive.print
      println(Console.BLINK + Console.RED +"       GAME OVER" + Console.RESET)

      SecretHamletService.submitToLeaderBoard(name, score)

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
      case 'a' | 'D' => staticTets ++ userControlledTets.map(tetromino => tetromino.moveLeft(blocks))
      case 'd' | 'C' => staticTets ++ userControlledTets.map(tetromino => tetromino.moveRight(blocks))
      case 'w' | 'A' => Rotate.rotate(blocks)
      case _   => blocks
    }
    this.copy(blocks = newBlocks)
  }

  def collisionCheck: Board = {
    val blocks = this.blocks

    val movingTets = blocks.filter(t =>  t.canFall)
    val playerTets = blocks.filter(t =>  t.userCanControl)
    val uncontrolledTetsIndexes = blocks.filterNot(_.userCanControl).map(_.index)
    val nonMovingTetsIndexes = blocks.filterNot(t =>  t.canFall).map(_.index).toSet
    val movingTetsFutureIndexes = movingTets.map(_.index+10).toSet

    def hitTheBottomCC(T: List[Tetromino])= {T.map(t => if(t.index+10>149) t.freeze else t)}
    def blockBelowCC(T: List[Tetromino])= {if (nonMovingTetsIndexes.intersect(movingTetsFutureIndexes).nonEmpty){T.map(_.freeze)} else T}
    def rightWallCC(T: List[Tetromino])= {if (playerTets.exists(t => (t.index+1)%10 == 0)){T.map(t => t.copy(canMoveRight = false))}else T}
    def leftWallCC(T: List[Tetromino])= {if (playerTets.exists(t => (t.index-1)%10 == 9 || (t.index<0) && (t.index-1)%10 == -1 || t.index == 0))
    {T.map(_.copy(canMoveLeft = false))} else T}
    def blockToLeftCC(T: List[Tetromino])= {if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.index-1))){T.map(_.copy(canMoveLeft = false))} else T}
    def blockToRightCC(T: List[Tetromino])= {if (playerTets.exists(t => uncontrolledTetsIndexes.contains(t.index+1))){T.map(_.copy(canMoveRight = false))} else T}

    def allCC = hitTheBottomCC _ andThen blockBelowCC  andThen rightWallCC andThen leftWallCC andThen blockToLeftCC andThen blockToRightCC

    this.copy(blocks = allCC(blocks))
  }



}