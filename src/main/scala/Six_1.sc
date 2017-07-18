object Six_1 {
  val sequence = Seq(1, 2, 3)

  sequence.apply(0)
  sequence(0)

  sequence.head
  sequence.tail

  sequence.headOption
  Seq().headOption

  sequence.length

  sequence.contains(2)

  sequence.find(_ == 3)
  sequence.filter(_ > 1)

  sequence.:+(4)

  sequence :+ 4

  sequence.+:(0)

  0 +: sequence
  sequence ++ Seq(4, 5, 6)

  val list = 1 :: 2 :: 3 :: Nil
  4 :: 5 :: list

  import scala.collection.immutable._
  import scala.collection.immutable.Vector.apply

  Vector(1, 2, 3)
  Queue(1, 2, 3)
  apply(1, 2, 3)



}
