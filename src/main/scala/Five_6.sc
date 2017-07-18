object Five_6 {
  sealed trait Maybe[+A]
  final case class Full[A](value: A) extends Maybe[A]
  final case object Empty extends Maybe[Nothing]

  val possible:Maybe[Int] = Empty

}