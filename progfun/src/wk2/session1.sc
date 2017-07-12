object session1 {
  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    loop(a, 0)
  }
  sum(x => x * x, 3, 5)

  def sumC(f: Int => Int)(a:Int, b:Int): Int = {
    if (a > b) 0 else f(a) + sumC(f)(a+1, b)
  }

  def product(f: Int => Int)(a:Int, b:Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a+1, b)
  }
  product(x => x * x)(1, 3)

  def fact(a: Int) = product(x => x)(1, a)
  fact(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a:Int, b:Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1,b))
  }
  mapReduce(x => x, (p,q) => p*q, 1)(1,5)
}