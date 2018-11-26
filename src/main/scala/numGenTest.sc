
val seed = 1111111111

val bin = "1111111111" // seed.toBinaryString

val gen = bin.takeRight(5)+bin.take(5)

Integer.parseInt(bin, 2)


case class Random() {

  private var current = 0

  def nextInt() = {
      var str = binPrep(current)
    val l: List[Int] = current.toString.toList.map(_.asDigit)
    l.foreach{i =>
      str = (str.substring(0, i) + binSwap(str(i)) + str.substring(i+1, 10)).reverse
      str = str.map(c => binSwap(c)).mkString
    }
      current = Integer.parseInt(str, 2)
    // l.sum%6
    current
  }

  def binSwap(str : Char): String ={
    str match {
      case '0' => "1"
      case '1' => "0"
    }
  }


  def binPrep(num: Int) = {
    var str = num.toBinaryString
    while(str.length < 10 ){
      str = "0"+  str
    }
    str
  }
}

val random = Random()

random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()
random.nextInt()