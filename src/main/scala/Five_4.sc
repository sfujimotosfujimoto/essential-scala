//("hi", 1)
//
//def tuplized[A, B](in: (A, B)) = in._1
//tuplized(("A", 1))
//
//(1, "a") match {
//  case (a, b) => a + b
//}
//
sealed trait Sum[A, B] {
  def fold[C](left:A => C, right: B => C):C =
    this match {
      case Left(a) => left(a)
      case Right(b) => right(b)
    }
}
final case class Left[A, B](value:A) extends Sum[A, B]
final case class Right[A, B](value:B) extends Sum[A, B]

Left[Int, String](1).value
Right[Int, String]("foo").value

val ex = Left(1, "hello")
val b = ex.fold[String](x => x.toString(), x => x.toString)

//
//val sum: Sum[Int, String] = Right("foo")
//
//sum match {
//  case Left(x) => x.toString
//  case Right(x) => x
//}
object Five_4 {
  sealed trait Maybe[A] {
    def fold[B](full: A => B, empty: B): B =
      this match {
        case Full(v) => full(v)
        case Empty() => empty
      }
  }
  final case class Full[A](value:A) extends Maybe[A]
  final case class Empty[A]() extends Maybe[A]


  val perhaps: Maybe[Int] = Empty[Int]()
}

