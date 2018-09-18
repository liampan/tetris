class Tetromino(shape: String, colour: String, private var index: Int) {

  var canMove = true

  var canFall = true

  def getIndex:Int = {
    this.index
  }

  def moveLeft ={
    if (canMove && (index-1)/10 == index/10 && index-1 > 0) {
      index = index - 1
    }
  }

  def moveRight ={
    if (canMove && (index+1)%10 != 0) {
      index = index + 1
    }
  }

  def fall(boardWidth: Int) ={
    if ((index+boardWidth) < 150 && canFall) {
      index = index + boardWidth
    } else {
      stopMove
    }
  }

  def restartGravity ={
    canFall = true
  }


  def stopMove ={
    canMove = false
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
