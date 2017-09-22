
object Lab11 {

  def fixedPoint(f: Double => Double)(startingGuess: Double): Double = {
    def inner (guess: Double) : Double = {
      //println(isCloseEnough(guess,f))
      if (isCloseEnough(guess,f)) guess
      else {
        inner(f(guess))
      }
    }
      inner(startingGuess)
  }
  def abs(x: Double): Double = if (x < 0) -x else x

  def isCloseEnough(guess: Double, f: Double => Double): Boolean = {
    abs(f(guess) - guess) < 0.0001
  }

  def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

  def sqrt(a: Double) = fixedPoint(averageDamp(x => a/x))(1.0)

  def main(args: Array[String]): Unit = {
    println(sqrt(2))
  }

}

