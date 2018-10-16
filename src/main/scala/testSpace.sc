case class TestTetromino(
                      shape: String,
                      colour: String,
                      index: Int,
                      userCanControl: Boolean = true,
                      canMoveLeft: Boolean = true,
                      canMoveRight: Boolean = true,
                      calFall: Boolean = true,
                      rotateState: Int = 0
                    ) {

  def rotate: TestTetromino = {
    val rotatedTet = this
    rotatedTet.rotated
  }

  private def rotated: TestTetromino ={
    this.copy(rotateState = if(this.rotateState == 3) 0 else this.rotateState+1)
  }

}

val b = TestTetromino("","",1)

b.rotateState
b.rotate.rotateState
b.rotate.rotate.rotateState
b.rotate.rotate.rotate.rotateState
b.rotate.rotate.rotate.rotate.rotateState


