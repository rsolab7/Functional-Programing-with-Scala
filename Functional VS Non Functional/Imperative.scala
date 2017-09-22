object Imperative {

  def fizzBuzz(n: Int) = {
    var i = 1;
    while (i <= n) {
      if (i % 5 == 0 && i % 3 == 0) {
          println("FizzBuzz")
      } else if (i % 5 == 0) {
          println("Buzz")
      } else if (i % 3 == 0) {
          println("Fizz")
      } else {
          println(i)
      }
      i += 1
    }
  }

  def fact(x: Int): Int = {
    if (x ==0){
      return 1
    }else{
      return x * fact(x-1)
    }
  }

  

  def paranMatch(chars: Array[Char]): Boolean = {
    //var len: Int = Array.lenght
    //println(Array.lenght)
    var i: Int = 0
    var count = 0
    while ( i < chars.length){
      if (count < 0){
        return false
      }
      if (chars(i) == '('){
        count = count + 1
      }
      else if (chars(i).equals(')')){
        count = count - 1;
      }
      i +=1;
    }//While close
    //println("Count  "+ count)
    if (count == 0)
      true
    else
      false

  }


  def main(args: Array[String]) = {
    fizzBuzz(20)

//    println(fact(0))
//    println(fact(5))
//    println(fact(11))

     assert(fact(0) == 1)
     assert(fact(5) == 120)
     assert(fact(11) == 39916800)




   // println(paranMatch(("(if (zero? x) max (/ 1 x))").toArray) == true)
   // println(paranMatch("xyz (blah (y) blah). (???)".toArray) == true)
   //println(paranMatch("())(".toArray))
   // println(paranMatch("hello :)".toArray) == false)

     assert(paranMatch("(if (zero? x) max (/ 1 x))".toArray) == true)
     assert(paranMatch("xyz (blah (y) blah). (???)".toArray) == true)
     assert(paranMatch("())(".toArray) == false)
     assert(paranMatch("hello :)".toArray) == false)
  }

}

