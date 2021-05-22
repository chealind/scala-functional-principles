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

  def pascal(c: Int, r: Int): Int = {
    if (c == r || c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def checkOpen(chs: List[Char], openCount: Int): Boolean = {
      if (chs.isEmpty) openCount == 0
      else if (openCount < 0) false
      else chs.head match {
        case '(' => checkOpen(chs.tail, openCount + 1)
        case ')' => checkOpen(chs.tail, openCount - 1)
        case _ => checkOpen(chs.tail, openCount)
      }
    }

    checkOpen(chars, 0)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
