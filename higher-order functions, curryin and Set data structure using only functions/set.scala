import scala.util.matching.Regex.Match

object Newtons {
  type  Set = Int => Boolean


  def  contains(set: Set, elem:  Int ):  Boolean  = {
    set(elem)
  }

  def singletonSet (elem: Int): Set = {
      Set(elem)
  }

  def union (s: Set, t: Set): Set = {
    def theUnion(i: Int): Boolean = {
      s(i) || t(i)
    }
    theUnion
  }

  def intersect (s: Set, t: Set): Set = {
    def inter(i: Int): Boolean = {
      s(i) && t(i)
    }
    inter
  }

  def difference (s: Set, t: Set): Set = {
    def diff(i: Int): Boolean = {
      s(i) != t(i)
    }
    diff
  }

  def filter(s: Set , p: Int => Boolean ):  Set  = {
    def inner (i: Int): Boolean ={
      p(i)
    }
    inner
  }

  def iseven(a: Int): Boolean = (a % 2) == 0

  def  forall(s:  Set , p:  Int => Boolean ):  Boolean = {
    def inner (i:Int):Boolean = {
      if (contains(s,i)){
        if (p(i) == false) {
          p(i)
        }
        else if(i <= -1000) true
        else inner(i-1)
      }else if (i <= -1000) true
      else inner(i-1)
    }
    inner(1000)
  }

  def  map(s:  Set , f:  Int => Int ):  Set  = {
    def themap(i:Int, g: Set):Set = {
      if (i <= -1000){
        g
      }else if(contains(s,i)){
        themap(i-1,union(g,singletonSet(f(i))))
      }else
        themap(i-1,g)
    }
    val t = singletonSet(0)
    //HOW CAN I PASS A NEW EMPTY SET? || now T is a set with the element 0
    themap(1000,t)
  }

//  import scala.annotation._
//
//    def startswith(string: String, start: String): Boolean = {
//      def inner(i: Int):Boolean = {
//        if(i==start.length()) true
//        else if (string.charAt(i) != start.charAt(i)) false
//        else inner (i+1)
//      }
//      inner(0)
//    }
//
//    def collectAword(list:List[String]):List[String]={
//      def inner (i: Int,l: List[String]):List[String] = {
//        if (i >= list.length) l
//        else if (startswith(list[i],"a") l.append(list[i])
//        else inner(i+1,l)
//      }
//      inner(0,List)
//    }




  def main(args: Array[String]): Unit = {
    println(startswith("Hello world","Hell"))


    val s1 = Set(1,2,3)
    assert(contains(s1,1) == true)
    val ss = singletonSet(7)
    assert(contains(ss,7) == true)
    val  setOfOne = singletonSet(2) // Creates a set of single int, 2.
    assert(contains(setOfOne, 2) == true)
    assert(contains(setOfOne, 1) == false)
    val u = union(s1,ss)
    assert(contains(u,7) == true)
    val i = intersect(s1,setOfOne)
    assert(contains(i,2))
    val d = difference(s1,setOfOne)
    assert(contains(d,2)==false)

    val  a = singletonSet(1)
    val  b = union(a, singletonSet(2))
    val  c = union(singletonSet(3), singletonSet(4))
    assert(contains(a, 1) == true)
    assert(contains(union(b, c), 3) == true)
    assert(contains(intersect(a, b), 1) == true)
    assert(contains(intersect(a, b), 2) == false)
    assert(contains(difference(b, a), 2) == true)
    assert(contains(difference(b, a), 1) == false)

    val g = union(b,singletonSet(3))
    val f = union(g,singletonSet(4))
    val  even = filter(f,iseven)

    assert(contains(even, 1) == false)
    assert(contains(even, 2) == true)
    assert(contains(even, 3) == false)
    assert(contains(even, 4) == true)

    assert(forall(f, x => x < 10) == true)
    assert(forall(f, x => x % 2 == 0) == false)

    val  newSet = map(f, x => x * x)
    assert(contains(newSet, 9) == true)
    assert(contains(newSet, 16) == true)
    assert(contains(newSet, 3) == false)


  }
}