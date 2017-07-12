import scala.annotation.tailrec

/*  / sfujimoto: 2017/07/11 21:49 */

object Five_1 extends App{
  sealed trait Result[A]
  case class Success[A](result:A) extends Result[A]
  case class Failure[A](reason:String) extends Result[A]

  sealed trait LinkedList[A] {
    def apply(num:Int):Result[A] =
      this match {
        case Pair(hd, tl) => if (num == 0) Success(hd) else tl(num -1)
        case End() =>
          Failure("It wasn't there!!")
      }
    def length:Int =
      this match {
        case Pair(hd, tl) => 1 + tl.length
        case End() => 0
      }

    def contains(value:A ):Boolean =
      this match {
        case Pair(hd, tl) =>
          if(hd == value) true
          else tl.contains(value)
        case End() => false
      }
  }
  final case class End[A]() extends LinkedList[A]
  final case class Pair[A](head:A, tail:LinkedList[A]) extends LinkedList[A]

  val example = Pair(1, Pair(2, Pair(3, End())))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)
  assert(example(0) == Success(1))
  assert(example(1) == Success(2))

//  def fold(end: Int, f: (Int, Int) => Int):Int =
//    this match {
//      case End() => end
//      case Pair(hd, tl) => f(hd, tl.fold(f, end))
//    }

}


