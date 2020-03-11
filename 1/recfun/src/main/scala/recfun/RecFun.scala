package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 | r == 0 || c >= r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def accum(x: List[Char], n: Int): Boolean = {
      if ((n < 0) || (x.isEmpty && n != 0))
        false
      else if (x.isEmpty && n == 0)
        true
      else if (x.head == '(')
        accum(x.tail, n + 1)
      else if (x.head == ')')
        accum(x.tail, n - 1)
      else
        accum(x.tail, n)
    }
    accum(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money <  0)
      0
    else if (coins.tail.isEmpty) {
      if (money % coins.head == 0) 1
      else 0
    }
    else {
        countChange(money, coins.tail) +
        countChange(money - coins.head, coins)
    }
  }
}