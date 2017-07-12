import scala.annotation.tailrec

sealed trait Result[A]
case class Success[A](result:A) extends Result[A]
case class Failure[A](reason:String) extends Result[A]

sealed trait LinkedList[A] {
  @tailrec
  def length:Int =
    this match {
      case Pair(hd, tl) => 1 + tl.length
      case End() => 0
    }
}
final case class End[A]() extends LinkedList[A]
final case class Pair[A](head:A, tail:LinkedList[A]) extends LinkedList[A]
