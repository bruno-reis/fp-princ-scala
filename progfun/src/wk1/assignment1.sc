import scala.annotation.tailrec

object assignment1 {

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def count(l: List[Char], open: Int): Int = l match {
      case Nil => open
      case h :: t => {
        if (open < 0) open
        else if ( h == '(') count(t, open + 1)
        else if ( h == ')') count(t, open - 1)
        else count(t, open)
      }
    }
    count(chars, 0) == 0
  }

  balance("()())".toList)

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  pascal(1,3)

  def countChange(money: Int, coins: List[Int]): Int = ???
}


