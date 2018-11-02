case class Tetromino(
                      shape: String,
                      colour: String,
                      index: Int,
                      userCanControl: Boolean = true,
                      canMoveLeft: Boolean = true,
                      canMoveRight: Boolean = true,
                      canFall: Boolean = true,
                      rotateState: Int = 0)
{

  def rotated: Tetromino ={
    this.copy(rotateState = if(this.rotateState == 3) 0 else this.rotateState+1)
  }

  def moveLeft(board: List[Tetromino]): Tetromino ={
    val uncontrolledTetsIndexes = board.filterNot(_.userCanControl).map(_.index)

    if (this.canMoveLeft && this.userCanControl && !uncontrolledTetsIndexes.contains(index-1)) {
      this.copy(canMoveRight = true, index = this.index -1)
    }else this
  }

  def moveRight(board: List[Tetromino]): Tetromino ={
    val uncontrolledTetsIndexes = board.filterNot(_.userCanControl).map(_.index)

    if (canMoveRight && userCanControl && !uncontrolledTetsIndexes.contains(this.index+1)) {
      this.copy(canMoveLeft = true, index = this.index +1)
    }else this
  }

  def fall(boardWidth: Int): Tetromino ={
    if ((index+boardWidth) < 150 && canFall) {
      this.copy(index = index + boardWidth)
    } else {
      this.freeze
    }
  }

  def restartGravity: Tetromino ={
    this.copy(
    canFall = true)
  }


  def freeze: Tetromino ={
    this.copy(
      userCanControl = false,
      canFall = false)
  }

  def getLook: String ={
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
