import scala.io.StdIn.{readLine readInt}
import scala.math._
import scala.collection._
import scala.io.PrintWriter
import scala.io.Source


object scalaTutorial {
  def  main(args: Array[String]) {
    var i = 0
//    do{
//      println(i)
//      i += 1
//    }while (i<= 2)
//
//    var j = 0
//    for (j <- 1 to 10)
//      println (j)
//
//
//    val randomLetter = "abcdefghjklifunhck"
//    for (i <- 0 until(randomLetter.length))
//      println(randomLetter(i))
//
//
//    val aList = List(1,2,3,4,5)
//    for (i <- aList){
//      println("list Items " + i)

//    var evenList = for {i <- 1 to 20
//                        if (i % 2) == 0
//    }yield i
//    for (i <- evenList)
//      println(i)
//    def printPrimes() {
//    val primeList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
//    for (i <- primeList) {
//      if (i == 11) {
//        return
//      }
//      if (i != 1) {
//        println(i)
//      }
//    }
//    }
//  printPrimes()


    var numberGuess = 0
    do{
      print ("Guess a Number ")
      numberGuess = readLine.toInt

    }while (numberGuess != 15)
    printf("you guess the secreat number %d\n",15)





  }
}