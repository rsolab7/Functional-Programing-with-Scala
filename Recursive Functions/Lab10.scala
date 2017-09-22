import scala.annotation._

/*
Tail calls can be implemented without adding a new stack frame to the call stack.
Most of the frame of the current procedure is no longer needed, and can be
replaced by the frame of the tail call, modified as appropriate
 */

object Lab10 {

  //Here's an example of how to use @tailrec
  //Tailec - A function that call itself as last action
  def factorial(n: BigInt): BigInt = {
    @tailrec
    def inner(n: BigInt, product: BigInt): BigInt =
      if (n <= 0) product 
      else inner(n - 1, product * n)

    inner(n, 1)
  }

  // Does not consider -1 as error {)()(}
//  def paranMatch(chars: Array[Char]): Boolean = {
//    @tailrec
//    def counter(count: Int, i: Int): Int = {
//      if (i==chars.length)
//        count
//      else if(chars(i) == '(')
//        counter(count + 1, i + 1)
//      else if (chars(i) == ')')
//        counter(count - 1, i + 1)
//      else
//        counter(count, i + 1)
//    }
//
//    if (counter(0,0) == 0 && isneg())
//      true
//    else
//      false
//  }

  def paranMatch(chars: Array[Char]): Boolean = {
    @tailrec
    def counter(chars: Array[Char], i: Int): Boolean = {
      if (chars.isEmpty) i == 0
      else if (chars.head == '(') counter(chars.tail, i + 1)
      else if (chars.head == ')') i > 0 && counter(chars.tail, i - 1)
      else counter(chars.tail,i)
    }
    counter(chars,0)
  }


  def fib(n: Int): Int = {
    if (n <= 2) 1
    else
      fib(n-1) + fib(n-2)
  }

  def isPalindrome(chars: Array[Char]): Boolean = {
    @tailrec
    def inner(chars: Array[Char], i:Int, j:Int ): Boolean ={
      if (i == j) {
        true
      }else if (chars(i) == chars(j)){
        inner(chars,i+1, j-1)
      }
      else false
    }
    inner(chars,0,chars.length-1)
  }

  def power(x: BigInt, n: BigInt): BigInt = {
    @tailrec
    def inner(x: BigInt,y: BigInt, n: BigInt): BigInt = {
      if (n <= 1) x
      else
        inner(x * y,y,n-1)
    }
    inner(x,x,n)
  }

  def hanoi(n: BigInt): BigInt = {
    @tailrec
    def inner (calc: BigInt, m: BigInt): BigInt = {
      if(m <= 0)  calc - 1
      else
        inner ((2*calc),m-1)
    }
    inner(1,n)
  }

  def id (x: Int): Int ={
    x
  }
  def cube (x: Int): Int ={
    x * x * x
  }

//  def sum(f: Int => Int, a: Int ,b: Int): Int = {
//    @tailrec
//    def inner (a:Int, sum: Int): Int =
//      if (a>=b)  sum
//      else inner(a+1, f(a)+sum)
//
//    inner(a,0)
//  }
//  def sum(f: Int => Int)(a: Int)(b: Int): Int = {
//    @tailrec
//    def inner (a:Int, sum: Int): Int =
//      if (a>=b)  sum
//      else inner(a+1, f(a)+sum)
//
//    inner(a,0)
//  }
def sum(f: Int => Int):(Int, Int) => Int = {
  @tailrec
  def inner (a:Int, sum: Int): Int =
    if (a>=b)  sum
    else inner(a+1, f(a)+sum)

  inner(a,0)
}


  def main(args: Array[String]) = {
      //println(factorial(5))
      assert(paranMatch("(a + b) - ((c * d) + e)".toArray)== true)
      assert(paranMatch("())(".toArray)== false)
      assert(paranMatch("(a + b) - ((c * d) + e)".toCharArray))
      println("Fib of 10: "+fib(10))
      assert(fib(10) == 55)
      assert(isPalindrome("racecar".toCharArray)==true)
      assert(isPalindrome("hola".toCharArray)== false)
      println("2^20: "+power(2,20))
      assert(power(2, 10) == 1024)
      println("Hanoi 1: "+hanoi(1))
      println("Hanoi 14: "+hanoi(14))
      assert(hanoi(10) == 1023 )
      assert(hanoi(15) == 32767)



        def sumcube = sum(cube)
        println(sumcube(1,5))


//      //Returns Int
//      println("Sum "+sum(id)(1)(5))
//      //retuns a f: ((Int,Int) => Int)
//      def sumcube = sum(cube)_
//      //retuns a f: (Int => Int)
//      def sumcube1 = sumcube(1)
//      println(sumcube(1)(5))
//      println(sumcube1(5))


//      println("Cube " + sum(cube,1,2))
//      println("Sum" + sum((x: Int) => x,1,10))
//      println("Cube " + sum((x: Int) => x*x*x,1,10))
  } 
}


