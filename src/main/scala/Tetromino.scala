class Tetromino(shape: String, colour: String, private var index: Int) {

  var userCanControl = true

  var canMoveLeft = true

  var canMoveRight = true

  var canFall = true

  def getIndex:Int = {
    this.index
  }


  def getShape:String = {
    this.shape
  }

  def moveLeft(board: List[Tetromino]) ={

    val uncontrolledTetsIndexes = board.filterNot(_.userCanControl).map(_.getIndex)
    val userControlledTets = board.filter(_.userCanControl)

    if (canMoveLeft && userCanControl && index-1 > 0 && !uncontrolledTetsIndexes.contains(index-1)) {
      userControlledTets.foreach(_.canMoveRight = true)
      index = index - 1
    }
  }

  def moveRight(board: List[Tetromino]) ={
    val uncontrolledTetsIndexes = board.filterNot(_.userCanControl).map(_.getIndex)
    val userControlledTets = board.filter(t => t.userCanControl || t.canFall)

    if (canMoveRight && userCanControl && !uncontrolledTetsIndexes.contains(index+1)) {
      userControlledTets.foreach(_.canMoveLeft = true)
      index = index + 1
    }
  }

  def fall(boardWidth: Int) ={
    if ((index+boardWidth) < 150 && canFall) {
      index = index + boardWidth
    } else {
      freeze
    }
  }

  def restartGravity ={
    canFall = true
  }


  def freeze ={
    userCanControl = false
    canFall = false
  }

  def getLook ={
    val consoleColor = this.colour match {
      case "red"     => s"${Console.RED}"
      case "green"   => s"${Console.GREEN}"
      case "blue"    => s"${Console.BLUE}"
      case "magenta" => s"${Console.MAGENTA}"
      case "cyan"    => s"${Console.CYAN}"
      case _         => s"${Console.YELLOW}"
    }

    consoleColor+s"■${Console.RESET}"
  }

}
