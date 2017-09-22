object Amazon {

  case class Author(penName: String, realName: String)
  case class Publisher(name: String)
  case class Book(title: String, author: List[Author], publisher: Publisher, year: Int, price: Double)
  case class Series(name: String, books: List[Book])

  // Find all books that written by an author
  // The authorName should match either pen or real name.
  // If there are no books that matches the author, return an empty list
  def findAll(books: List[Book], authorName: String): List[Book] = {
    books.filter(books => books.author.exists(a => a.penName == authorName || a.realName == authorName))
  }

//  // Find all books that was published on a given year.
//  // If no books were published that year, you should get an empty list
  def findAll1(books: List[Book], year: Int): List[Book] = {
    books.filter(books => books.year == year)
  }
//
  // Find average price of all the books by the author
  def findAveragePriceAuthor(books: List[Book], author: Author): Double = {
    val price =  books.filter(books => books.author.exists(a => a.penName == author.penName ||
      a.realName == author.realName)).map(books => books.price)
    val tot = price.sum
    val size = price.length
    tot/size
  }

  // Find average price of all the books given a publisher
  def findAveragePricePub(books: List[Book], pub: Publisher): Double = {
    val price = books.filter(books => books.publisher == pub).map(books => books.price)
    val tot = price.sum
    val size = price.length
    tot/size
  }

//  // Generalize above two functions into one that finds a average price among books that matches a predicate
  def findAverage(books: List[Book], p: Book => Boolean): Double = {
    val price = books.filter(p).map(books => books.price)
    val tot = price.sum
    val size = price.length
    tot/size
  }

  // Given a list of books, produce a list of unique authors
  // You should use .toSet method on List
//  def authors(books: List[Book], author: Author): Set[Author] = {
//    books.filter(books => books.author.exists(a => a.penName == author.penName ||
//      a.realName == author.realName)).map(b => b.author).toSet
//  }

//  // Given a list of books, produce a list of unique publisher
//  // You should use .toSet method on List
  def publishers(books: List[Book], publisher: Publisher): Set[Publisher] = {
    books.filter(books => books.publisher == publisher).map(books => books.publisher).toSet
}

//  // Generalize above two functions.
//  // You should use .toSet method on List
  def toSet[A](books: List[Book], p: A => Boolean): Set[A] = {
  books.filter(p).map(books => books.A).toSet
}

//  // Given a book series, find the total cost of the collection
//  def total(series: Series): Double = {
//    series
//  }


//  // Given a book series, produce a list of unique authors that contributed
//  // You should use .toSet method on List
//  def authors(series: Series): Set[Author] = ???


  // Given a list of books, produce a list of unique authors
  // You should use .toSet method on List
  def authors(books: List[Book]): Set[Author] =
  books.flatMap(b => b.author).toSet

  // Given a list of books, produce a list of unique publisher
  // You should use .toSet method on List
  def publishers(books: List[Book]): Set[Publisher] =
  books.map(b => publisher).toSet

  // Generalize above two functions.
  // You should use .toSet method on List
  def toSet[A](books: List[Book], p: Book => List[A]): Set[A] =
  books.flatMap(p).toSet

  // def authors and def publishers can be re-written like this:
  def authors2(books: List[Book]): Set[Author] = toSet(books, b => b.author)
  def publishers2(books: List[Book]): Set[Publisher] = toSet(books, b => List(b.publisher))

  def main(args: Array[String]): Unit = {
    val huckfinn = Book(
      "The Adventures of Huckleberry Finn",
      List(Author("Mark Twain", "Samuel Langhorne Clemens")),
      Publisher("Chatto & Windus"), 1885, 5.40)

    val design = Book(
      "Design Patterns: Elements of Reusable Object-Oriented Software",
      List(
        Author("Erich Gamma", "Erich Gamma"),
        Author("Richard Helm", "Richard Helm"),
        Author("Ralph Johnson", "Ralph Johnson"),
        Author("John Vlissides", "John Vlissides")
      ), Publisher("Addison-Wesley"), 1994, 28.11)

    val lotr = Series("The Lord of the Rings", List(
      Book(
        "The Followship of the Ring",
        List(Author("J. R. R. Tolkien", "John Ronald Reuel Tolkien")),
        Publisher("George Allen & Unwin"), 1954, 7.99)
    ))

    val amazon = List(huckfinn,design)

    println(findAll(amazon,"Richard Helm"))
    println("-----")
    println(findAll1(amazon,1994))
    println("-----")
    println(findAll1(amazon,1885))
    println("-----")
    println("Price of design based on author  "+findAveragePriceAuthor(amazon,design.author(0)))
    println("Design price based on publisher: "+findAveragePricePub(amazon,design.publisher))
    println("Design price based on price: "+findAverage(amazon,Books => Books.price == design.price))
    println("Huckfin price based on title: "+findAverage(amazon,Books => Books.title == huckfinn.title))
    //println(publishers("Publisher of huckfinn "+amazon,huckfinn.publisher))
    println("find Huckfin price based on Author "+findAverage(amazon,Book => Book.author == huckfinn.author))
    assert(findAll(amazon, "Richard Helm") == List(design))

    //assert(total(lotr) == 7.99)

    //You should write more tests

  }

}

