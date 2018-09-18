class Tetromino(shape: String, colour: String, private var index: Int) {

  var userCanControl = true

  var canMoveLeft = true

  var canMoveRight = true

  var canFall = true

  def getIndex:Int = {
    this.index
  }

  def moveLeft(board: List[Tetromino]) ={
    val uncontrolledTetsIndexes = board.filterNot(_.userCanControl).map(_.getIndex)
    val userControlledTets = board.filter(_.userCanControl)
    if (uncontrolledTetsIndexes.contains(index-1)){userControlledTets.foreach(_.canMoveLeft = false)}


    //TODO second spawn has no userControl
    val notOnLeftEdge = (index-1)/10 == index/10
    if (!notOnLeftEdge){userControlledTets.foreach(_.canMoveLeft = false)}

    if (canMoveLeft && userCanControl && notOnLeftEdge && index-1 > 0 && !uncontrolledTetsIndexes.contains(index-1)) {
      userControlledTets.foreach(_.canMoveRight = true)
      index = index - 1
    }
  }

  //TODO this has bug when hitting the Right wall.

  def moveRight(board: List[Tetromino]) ={
    val uncontrolledTetsIndexes = board.filterNot(_.userCanControl).map(_.getIndex)
    val userControlledTets = board.filter(_.userCanControl)
    if (uncontrolledTetsIndexes.contains(index+1)){userControlledTets.foreach(_.canMoveRight = false)}

    val notOnRightEdge = (index+1)%10 != 0
    if (!notOnRightEdge){userControlledTets.foreach(_.canMoveRight = false)}

    if (canMoveRight && userCanControl && notOnRightEdge && !uncontrolledTetsIndexes.contains(index+1)) {
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
    canMoveRight = false
    canMoveLeft = false
    canFall = false
  }

  def getLook ={
    val consoleColor = this.colour match {
      case "red" => s"${Console.RED}"
      case "green" => s"${Console.GREEN}"
      case "blue" => s"${Console.BLUE}"
      case _ => s"${Console.YELLOW}"
    }

    consoleColor+s"■${Console.RESET}"
  }

}
