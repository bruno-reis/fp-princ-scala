import math.abs

object session2 {
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x-y) / x) x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def loop(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else loop(next)
    }
    loop(firstGuess)
  }
  fixedPoint(x => 1.0 + x / 2.0)(1.0)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2.0
  
  def sqrt(x: Double) =
    fixedPoint(averageDump(y => x / y))(1)
  sqrt(2)
}