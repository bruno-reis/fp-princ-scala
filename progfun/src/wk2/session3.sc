case class Rational(x: Int, y:Int) {
  require( y != 0, "Denominator must be nonzero")

  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
  private val g = gcd(x,y)
  val numer: Int = x / g
  val denom: Int = y / g

  override def toString: String = numer + "/" + denom

  def + (that: Rational) = {
     Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  }

  def unary_- : Rational = Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def < (that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this.<(that)) that else this

}

val x = Rational(1,2)
val y: Rational =  Rational(2,3)
x + y
x - y
x < y

