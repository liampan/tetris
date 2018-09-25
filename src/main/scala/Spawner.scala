import scala.util.Random

object Spawner {

  //TODO :
  // + add rotation fuction - Will need extra collision detection

  def spawn = {
    val shapes = List(spawnSquare, spawnTShape, spawnLShape, spawnJShape, spawnLine, spawnSShape, spawnZShape)
    shapes(Random.nextInt(shapes.length))
  }

  private def spawnSShape = {
    val colour = generateColour
    val name = "Sshape"
    val indexes = List(-37, -27, -26, -16)
    indexes.map(new Tetromino(name, colour, _))
  }

  private def spawnZShape = {
    val colour = generateColour
    val name = "Zshape"
    val indexes = List(-36, -27, -26, -17)
    indexes.map(new Tetromino(name, colour, _))
  }

  private def spawnSquare = {
    val colour = generateColour
    val name = "square"
    val indexes = List(-26, -25, -16, -15)
    indexes.map(new Tetromino(name, colour, _))
  }

  private def spawnTShape = {
    val colour = generateColour
    val name = "Tshape"
    val indexes = List(-36, -27, -26, -25)
    indexes.map(new Tetromino(name, colour, _))
  }

  private def spawnLine = {
    val colour = generateColour
    val name = "line"
    val indexes = List(-36, -26, -16, -6)
    indexes.map(new Tetromino(name, colour, _))
  }

  private def spawnLShape = {
    val colour = generateColour
    val name = "Lshape"
    val indexes = List(-37, -36, -26, -16)
    indexes.map(new Tetromino(name, colour, _))
  }

  private def spawnJShape = {
    val colour = generateColour
    val name = "Jshape"
    val indexes = List(-36, -35, -26, -16)
    indexes.map(new Tetromino(name, colour, _))
  }

  private def generateColour: String = {
    val colorList = List("red", "blue", "green", "magenta", "cyan", "yellow")
    colorList(Random.nextInt(colorList.length))
  }
}
