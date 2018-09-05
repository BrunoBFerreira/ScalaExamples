package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r == 0 || c == 0 || c == r)
        1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIter(chars: List[Char], acc: Int): Boolean = {
        if(chars.isEmpty)
          acc == 0
        else {
          if (acc < 0)
            false
          else balanceIter(chars.tail, acc + charVal(chars.head))
        }
      }
      def charVal(c: Char) = c match{
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
      balanceIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countLoop(money: Int, coins: List[Int]): Int = {
        if(money == 0)
          1
        else {
          if(coins.isEmpty || money < 0)
            0
          else
            countLoop(money - coins.head, coins) + countLoop(money, coins.tail)
        }
      }

      if(money == 0)
        0
      else
        countLoop(money, coins.sorted)
    }

  }
