import scala.util.Random

object Spawner {

  //TODO :
  // + add other tetrinmo shapes, J, S & Z **DONE**
  // move printer to seperate class **DONE**
  // + add rotation fuction - Will need extra collision detection
  // FIX movement bug as it shape crossed the spawn/board line, when shape half way accross and moves only half shape responds.

  def spawn ={
    val shapes = List(spawnSquare,spawnTShape,spawnLShape,spawnJShape,spawnLine,spawnSShape,spawnZShape)
    shapes(Random.nextInt(shapes.length))
  }

  private def spawnSShape={
    val colour = generateColour
    List(
      new Tetromino("S", colour, -37),
      new Tetromino("S", colour, -27), new Tetromino("S", colour, -26),
                                       new Tetromino("S", colour, -16))
  }

  private def spawnZShape={
    val colour = generateColour
    List(
                                       new Tetromino("Z", colour, -36),
      new Tetromino("Z", colour, -27), new Tetromino("Z", colour, -26),
      new Tetromino("Z", colour, -17))
  }

  private def spawnSquare={
    val colour = generateColour
    List(
      new Tetromino("square", colour,-26),new Tetromino("square", colour, -25),
      new Tetromino("square", colour, -16),new Tetromino("square", colour, -15))
  }

  private def spawnTShape ={
    val colour = generateColour
    List(                                   new Tetromino("Tshape", colour, -36),
      new Tetromino("Tshape", colour, -27), new Tetromino("Tshape", colour, -26), new Tetromino("Tshape", colour, -25))
  }

  private def spawnLine ={
    val colour = generateColour
    List(
      new Tetromino("line", colour, -36),
      new Tetromino("line", colour, -26),
      new Tetromino("line", colour, -16),
      new Tetromino("line", colour, -6))
  }

  private def spawnLShape ={
    val colour = generateColour
    List(
      new Tetromino("L", colour, -37), new Tetromino("L", colour, -36),
                                       new Tetromino("L", colour, -26),
                                       new Tetromino("L", colour, -16))
  }

  private def spawnJShape ={
    val colour = generateColour
    List(
      new Tetromino("J", colour, -36), new Tetromino("J", colour, -35),
      new Tetromino("J", colour, -26),
      new Tetromino("J", colour, -16))
  }

  private def generateColour: String ={
    val colorList = List("red", "blue", "green", "magenta", "cyan", "yellow")
    colorList(Random.nextInt(colorList.length))
  }
}
