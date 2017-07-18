/*  / sfujimoto: 2017/07/16 12:41 */

object Five_6 extends App{
  sealed trait Maybe[A]
  final case class Full[A](value: A) extends Maybe[A]
  final case object Empty extends Maybe[Nothing]

}
