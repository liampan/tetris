
def divisor(int: Int) = {
  val start = 4117
  var holding = start


  def nextInt() = {
    val working = holding
    holding = (working % 593) * 10
    working / 7
  }

  val runner = List.range(1, 10000)
  var fullList = List.range(1, start)

  runner.foreach(i => fullList = fullList.filterNot(_ == nextInt()))

  val removed = start - fullList.length

  def percentage = {
    val percentageRaw = (removed.toDouble / start) * 100
    percentageRaw - (percentageRaw % 0.01)
  }

 s"${nextInt().toBinaryString.map(_.asDigit).sum} -> $percentage% \n"
}

divisor(1)