object rationals {
  val x = new Rational(1,2)
  val y: Rational = new Rational(2,3)
  x + y
  x - y
  x < y
}

class Rational(x: Int, y:Int) {
  require( y != 0, "Denomitator must be nonzero")

  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
  private val g = gcd(x,y)
  val numer = x / g
  val denom = y / g

  override def toString: String = numer + "/" + denom

  def + (that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  }

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this.<(that)) that else this

}
