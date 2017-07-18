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
  final case class Empty[A] () extends Maybe[A]

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

//  list.map(_ * 2)
//  list.map(x => x * 3)
//
//  list.map(_ + 1)
//  list.map(_ / 3)


  //// 5.5.4.3
  val list2:List[Maybe[Int]] = List(Full(3), Full(2), Full(1))
//  list2.map(maybe => maybe flatMap { x => if(x % 2 == 0) Full(x) else Empty()})


  //// 5.5.4.4
  sealed trait Sum[A, B] {
    def map[C](fn: B => C): Sum[A, C] =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => Success(fn(b))
      }
    def flatMap[C](fn: B => Sum[A, C]) =
      this match {
        case Failure(a) => Failure(a)
        case Success(b) => fn(b)

      }
  }

  final case class Failure[A, B](value:A) extends Sum[A, B]
  final case class Success[A, B](value:B) extends Sum[A, B]

  val list3 = List(Success(2), Success(4), Failure(6))





}