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
        if (c == 0 || c == r) 1 else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean ={
       def getWeight(ch:Char):Int={
      ch match{
        case '(' => -1
        case ')' =>  1
        case default => 0
      }
    }
    
    def balanceInner(lst: List[Char]):Int={
      lst match{
          case x :: tail => {
            val result = getWeight(x)+balanceInner(lst.tail)
            if (result < 0) throw new IllegalStateException("wrong string")
            result
            } 
          case Nil => 0
        }
    }
    try{
      balanceInner(chars) == 0
    } catch {
              case e: IllegalStateException=> false
              }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
       def coinsInner(money:Int, banknotes:List[Int]):Int={
          if (money < 0 || banknotes.isEmpty) 0 
          else {
            if (money == 0 || (banknotes.length == 1 && money % banknotes.head == 0)) 1 
            else
            {
              coinsInner(money, banknotes.tail) + coinsInner(money - banknotes.head, banknotes)
            }
      }
  }
     coinsInner(money, coins.sortWith(_ > _).distinct)
 }
    
  }
