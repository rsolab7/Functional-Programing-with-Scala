object Lab12 {
  case object Nil extends List[Nothing]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  //This is a immutable data structure.
  //All the methods should not modify the existing List
  trait List[+A] {


    //Returns the (i + 1)th element in the list
    def get(i: Int): A = this match {
      case Nil => sys.error("Index out of bound")
      case Cons(head, tail) => if (i == 0) head else tail.get(i - 1)
    }

    //Prepends an element at the beginning of the list
    def prepend[B >: A](a: B): List[B] = Cons(a, this)

    //Appends an element at the end of the list
    //FIXME
    def append[B >: A](b: B): List[B] = {
      def inner(l1: List[B],l2: B): List[B] = l1 match {
        case Nil => Cons(b, Nil)
        case Cons(head,tail) => Cons(head,inner(tail,l2))
      }
      inner(this,b)
    }

    //Replaces the head of the list with a
    def setHead[B >: A](a: B): List[B] = this match {
      case Nil => sys.error("Index out of bound")
      case Cons(head,tail) =>  Cons (a,tail)
    }

    //Removes the last element in the list
    def init: List[A] = {
      def inner (list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(_,Nil) => Nil
        case Cons(head,tail) => Cons(head,inner(tail))
      }
      inner(this)
    }
    //Removes the first n elements in the list
    def drop(n: Int): List[A] = {
      def inner(n: Int, list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(head,tail) => {
          if(n<= 0) list
          else inner(n-1,tail)
        }
      }
      inner(n,this)
    }

    //Removes last (length - n) elements in the list
    def take(n: Int): List[A] = {
      def inner(n: Int, list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(_,Nil) => Nil
        case Cons(head,tail) => {
          println(list)
          if (n<=0){
            println("n=0")
            //fix return Statement
            list
          }
          else Cons(head,inner(n-1,tail))
        }
      }
      inner(n,this)
    }

    //Appends two lists together
    def append[B >: A](that: List[B]): List[B] = {
     def inner (list: List[A], tha:List[B] ): List[B] =list match {
        case Nil => that
        case Cons(head,tail) => Cons(head,inner(tail,that))
      }
      inner(this,that)
    }

    //Produces B by combining all the elments in the list using the function f starting with z then right most element
    def foldRight[B](z: B)(f: (A, B) => B): B = {
      def inner (list: List[A]): B = list match {
        case Nil => z
        case Cons(head,tail) => f(head,inner(tail))

      }
      inner(this)
    }


    //Produces B by combining all the elments in the list using the function f starting with z then left most element
    def foldLeft[B](z: B)(f: (B, A) => B): B = {
      def inner (list: List[A],b:B): B = list match {
        case Nil => b
        case Cons(head,tail) => inner(tail,f(b,head))
      }
      inner(this,z)
    }

      //returns true if all of the elements in the list returns true for the predicate f
    def forAll(f: A => Boolean): Boolean = {
      def inner(list: List[A]): Boolean = list match {
        case Nil => true
        case Cons(head,tail) => {
          if (f(head))  inner(tail)
          else false
        }
      }
      inner(this)
    }

    //returns the length of the list
    def length: Int = {
      def Inner(list: List[A],n: Int): Int = list match {
        case Nil => n
        case Cons(head,tail) => Inner(tail,n+1)
      }
      Inner(this,0)
    }

    //returns a reverse of the list (use foldLeft or foldRight)
    def reverse: List[A] = {
      this.foldLeft(Nil:List[A])((x, y) => Cons(y, x))
    }

    //Transform each of the element using f
    def map[B](f: A => B): List[B] = {
      this.foldRight(Nil:List[B])((head,tail) => Cons(f(head),tail))
    }

    //Transform each of the element using f then flatten
    def flatMap[B](f: A => List[B]): List[B] ={
      this.foldRight(Nil:List[B])((head,tail) => f(head).append(tail))
    }

    //returns a new list only containing elements that satisfies the predicate f
    def filter(f: A => Boolean): List[A] = {
      this.foldRight(Nil:List[A])((head,tail) => {
        if(f(head))  Cons(head,tail)
        else tail
      })
    }
  }
  object List {
    //Given a list of list, return a flattened list
    //List(List(1,2), List(3,4), 5).flatten == List(1,2,3,4,5)
    def flatten[A](list: List[List[A]]): List[A] = ???

    //Creates a new list of size n filled with element a
    def fill[A](n: Int)(a: A): List[A] = {
      val l = Nil
      def inner (n: Int,list: List[A]): List[A] = {
          if (n<=0) list
          else inner(n-1,Cons(a,list))
      }
      inner(n,l)
    }


    def apply[A](args: A*): List[A] = 
      if (args.isEmpty) Nil
      else Cons(args.head, apply(args.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    assert(List(1,2,3,4).get(2) == 3)
    assert(List(1,2,3).prepend(0) == List(0,1,2,3))
    assert(List(1,2).append(3) == List(1,2,3))
    assert(List(1,2,3).setHead(0) == List(0,2,3))
    assert(List(1,2,3).init == List(1,2))
    assert(List(1,2,3,4,5).drop(2) == List(3,4,5))
    assert(List(1,2,3,4).foldRight(0)((a, b) => a + b) == 10)
    assert(List(1,2,3,4).foldLeft(0)((b, a) => b + a) == 10)
    //assert(List(1,2,3,4,5).take(2) == List(1,2))
    assert(List(1,2).append(List(3,4)) == List(1,2,3,4))
    assert(List(1,2,3,4,5).forAll(x => x > 0) == true)
    assert(List(1,2,3,4,5).forAll(x => x < 3) == false)
    assert(List(1,2,3,4).length == 4)
    assert(List(1,2,3,4).reverse == List(4,3,2,1))
    assert(List(1,2,3).map(x => x * x) == List(1, 4, 9))
    assert(List(1,2,3).flatMap(x => List(x, x)) == List(1,1,2,2,3,3))
    assert(List(1,2,3,4,5).filter(x => x % 2 == 0) == List(2,4))
    assert(List.fill(3)(1) == List(1,1,1))
    //assert(List.flatten(List(List(1,2), List(3,4))) == List(1,2,3,4))


  }
}
