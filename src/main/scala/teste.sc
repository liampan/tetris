
case class Stem(length: Int)

//how a petal looks
case class petal(id : Int, colour: String)

// specific petals
val x = petal(0, "white")
val y = petal(1, "white")
val z = petal(2, "white")

//how a flower looks - petals and a stem
case class flower(petals : List[petal], stem: Stem)

val specificFlower = flower(List(x,y,z), Stem(10))
val s = specificFlower

val newS = specificFlower.copy(
  specificFlower.petals.map(x => x.copy(colour = "white"))
)

newS.hashCode() == s.hashCode()


object colour {

  val red = s"${Console.RED}"
}


case class Tet(private val _id : Int, index: Int, colour: String) {

  def moveLeft: Tet = {
    this.copy(index = this.index+1)
  }

  def getID: Int = {
    this._id
  }

  def moveRight: Tet = {
    this.copy(index = this.index-1)
  }
}

val tet = Tet(1, 5,colour.red)

tet.colour

tet.getID


case class Board(list: List[String], thing: String = "no", tick : Int = 0) {

   def tickOne = {
     this.copy(tick = this.tick+1)
   }
}


val board = Board(Nil)

val boardLater = board.tickOne

boardLater.tickOne