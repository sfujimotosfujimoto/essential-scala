
sealed trait TrafficLight

final case object Red extends TrafficLight
final case object Green extends TrafficLight
final case object Yellow extends TrafficLight

object Lights {
  def apply(tl:TrafficLight) =
    tl match {
      case Red => "It's red; Stop!!"
      case Green => "It's green; Walk!!"
      case Yellow => "It's yellow; You should stop!"
      case _ => "????"
    }
}

val r1 = Red

Lights(Red)

////////
sealed trait Calculator
final case class Succeed(num:Int) extends Calculator
final case class Failure(s:String) extends Calculator

object Calc {
  def add(a:Int, b:Int):Calculator =
    if (a + b > 0) Succeed(a + b)
    else Failure("Not Positive!")

}

Calc.add(4, -8)
Calc.add(3,4)


///////
final case class  BottledWater(
  size:Int,
  source:String,
  carbonated:Boolean
                              )
sealed trait Source
final case object Well extends Source
final case object Spring extends Source
final case object Tap extends Source






