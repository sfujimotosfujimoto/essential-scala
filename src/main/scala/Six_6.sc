import scala.util.Try
object Six_6 {
  val opt1 = Some(1)
  val opt2 = Some(2)
  val opt3 = Some(3)

  val seq1 = Seq(1)
  val seq2 = Seq(2)
  val seq3 = Seq(3)

  val try1 = Try(1)
  val try2 = Try(2)
  val try3 = Try(3)

  def add3(a:Option[Int], b:Option[Int], c:Option[Int]):Option[Int] = {
    for {
      a <- a
      b <- b
      c <- c
    } yield a + b + c
  }
  add3(opt1, opt2, opt3)

  def add3seq(a:Seq[Int], b:Seq[Int], c:Seq[Int]):Seq[Int] = {
    for {
      a <- a
      b <- b
      c <- c
    } yield a + b + c
  }
  add3seq(seq1, seq2, seq3)

  def add3try(a:Try[Int], b:Try[Int], c:Try[Int]):Try[Int] = {
    for {
      a <- a
      b <- b
      c <- c
    } yield a + b + c
  }
  add3try(try1, try2, try3)

}