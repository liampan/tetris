import scala.util.Random

object Spawner {

  //TODO :
  // + add other tetrinmo shapes, J, S & Z
  // + add rotation fuction - Will need extra collision detection
  // FIX movement bug as it shape crossed the spawn/board line, when shape half way accross and moves only half shape responds.

  def spawn ={
    Random.nextInt(4) match {
      case 0 => spawnLShape
      case 1 => spawnTShape
      case 2 => spawnLineVers
      case _ => spawnSquare
    }
  }

  def spawnSquare={
    val colour = generateColour
    List(
      new Tetromino("square", colour,-26),new Tetromino("square", colour, -25),
      new Tetromino("square", colour, -16),new Tetromino("square", colour, -15))
  }

  def spawnTShape ={
    val colour = generateColour
    List(                                   new Tetromino("Tshape", colour, -36),
      new Tetromino("Tshape", colour, -27), new Tetromino("Tshape", colour, -26), new Tetromino("Tshape", colour, -25))
  }

  def spawnLineVers ={
    val colour = generateColour
    List(
      new Tetromino("lineVer", colour, -36),
      new Tetromino("lineVer", colour, -26),
      new Tetromino("lineVer", colour, -16),
      new Tetromino("lineVer", colour, -6))
  }

  def spawnLShape ={
    val colour = generateColour
    List(
      new Tetromino("L", colour, -37), new Tetromino("L", colour, -36),
                                       new Tetromino("L", colour, -26),
                                       new Tetromino("L", colour, -16))
  }

  def generateColour: String ={
    val colorList = List("red", "blue", "green", "magenta", "cyan", "yellow")
    colorList(Random.nextInt(colorList.length))
  }
}
