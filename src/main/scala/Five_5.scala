/*  / sfujimoto: 2017/07/15 7:13 */

object Five_5 {
  ///// map
  sealed trait LinkedList[A] {
    def map[B](fn: A => B): LinkedList[B] =
      this match {
        case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
        case End() => End[B]()
      }
  }
  case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
  case class End[A]() extends LinkedList[A]

  sealed trait Maybe[A] {
    def flatMap[B](fn: A => Maybe[B]): Maybe[B] =
      this match {
        case Full(v) => fn(v)
        case Empty() => Empty()
      }
  }
  final case class Full[A](value:A) extends Maybe[A]
  final case class Empty[A]() extends Maybe[A]

  def mightFail1: Maybe[Int] = Full(1)
  def mightFail2: Maybe[Int] = Full(2)
  def mightFail3: Maybe[Int] = Empty()

  mightFail1 flatMap { x =>
    mightFail2 flatMap { y =>
      mightFail3 flatMap { z =>
        Full(x + y + z)
      }
    }
  }

  mightFail1 flatMap { x =>
    mightFail2 flatMap { y =>
      Full(x + y)
    }
  }
  //// ex 5.5.4.1
  val list: LinkedList[Int] = Pair(1, Pair(2, Pair(3, End())))

  list map (_ * 2)


}
