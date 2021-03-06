import scala.util.Random
import ShapeNames._

object Spawner{

  def spawn: List[Tetromino] = {
    val shapes = List(spawnSquare, spawnTShape, spawnLShape, spawnJShape, spawnLine, spawnSShape, spawnZShape)
    shapes(Random.nextInt(shapes.length))
  }

  private def spawnSShape = {
    val colour = generateColour
    val indexes = List(-37, -27, -26, -16)
    indexes.map(Tetromino(sShape, colour, _))
  }

  private def spawnZShape = {
    val colour = generateColour
    val indexes = List(-36, -27, -26, -17)
    indexes.map(Tetromino(zShape, colour, _))
  }

  private def spawnSquare = {
    val colour = generateColour
    val indexes = List(-26, -25, -16, -15)
    indexes.map(Tetromino(square, colour, _))
  }

  private def spawnTShape = {
    val colour = generateColour
    val indexes = List(-36, -27, -26, -25)
    indexes.map(Tetromino(tee, colour, _))
  }

  private def spawnLine = {
    val colour = generateColour
    val indexes = List(-36, -26, -16, -6)
    indexes.map(Tetromino(line, colour, _))
  }

  private def spawnLShape = {
    val colour = generateColour
    val indexes = List(-37, -36, -26, -16)
    indexes.map(Tetromino(lShape, colour, _))
  }

  private def spawnJShape = {
    val colour = generateColour
    val indexes = List(-36, -35, -26, -16)
    indexes.map(Tetromino(jShape, colour, _))
  }

  private def generateColour: String = {
    val colorList = List("red", "blue", "green", "magenta", "cyan", "yellow")
    colorList(Random.nextInt(colorList.length))
  }
}
