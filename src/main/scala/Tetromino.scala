class Tetromino(shape: String, colour: String,private var index: Int) {

  def getIndex():Int = {
    this.index
  }

  def moveLeft() ={
    if ((index-1)/10 == index/10 && index-1 > 0) {
      index = index - 1
    }
  }

  def moveRight() ={
    if ((index+1)%10 != 0) {
      index = index + 1
    }
  }

  def fall(boardWidth: Int) ={
    index = index + boardWidth
  }

  def getLook() ={
    val consoleColor = this.colour match {
      case "red" => s"${Console.RED}"
      case _ => s"${Console.YELLOW}"
    }

    consoleColor+s"■${Console.RESET}"
  }

}
