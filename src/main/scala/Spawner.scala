import scala.util.Random

object Spawner {

  def spawn ={
    Random.nextInt(4) match {
      case 0 => spawnLShape
      case 1 => spawnLineHoriz
      case 2 => spawnTShape
      case _ => spawnSquare
    }
  }

  def spawnSquare={
    val colour = generateColour
    List(new Tetromino("square", colour, 3),new Tetromino("square", colour, 4),
      new Tetromino("square", colour, 14),new Tetromino("square", colour, 13))
  }

  def spawnTShape ={
    val colour = generateColour
    List(
      new Tetromino("Tshape", colour, 3),
      new Tetromino("Tshape", colour, 4),
      new Tetromino("Tshape", colour, 14),
      new Tetromino("Tshape", colour, 5))
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
