import scala.util.Random

object Spawner {

  def spawn ={
    Random.nextInt(3) match {
      case 0 => spawnLShape
      case 1 => spawnLineHoriz
      case _ => spawnDot
    }
  }

  def spawnDot={
    val colour = generateColour
    List(new Tetromino("dot", colour, 5))
  }

  def spawnLineHoriz ={
    val colour = generateColour
    List(
      new Tetromino("lineHor", colour, 3),
      new Tetromino("lineHor", colour, 4),
      new Tetromino("lineHor", colour, 5),
      new Tetromino("lineHor", colour, 6))
  }

  def spawnLShape ={
    val colour = generateColour
    List(
      new Tetromino("L", colour, 5),
      new Tetromino("L", colour, 15),
      new Tetromino("L", colour, 25),
      new Tetromino("L", colour, 26))
  }

  def generateColour: String ={
    Random.nextInt(4) match {
      case 0 => "red"
      case 1 => "blue"
      case 2 => "green"
      case _ => "yellow"
    }
  }
}
