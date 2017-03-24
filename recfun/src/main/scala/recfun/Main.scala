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


  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      factorial(r) / (factorial(r - c) * factorial(c))
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance_recursive(chars: List[Char], num: Int): Boolean = {
        if (chars.isEmpty) num == 0
        else if (chars.head == '(') balance_recursive(chars.tail, num + 1)
        else if (chars.head == ')') {
          if (num > 0) balance_recursive(chars.tail, num - 1)
          else false
        }
        else balance_recursive(chars.tail, num)
      }

      return balance_recursive(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) return 0
      if (money == 0) return 1

      return countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
