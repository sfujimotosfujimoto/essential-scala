import java.util.Date

sealed trait TrafficLight {
  def next(tl:TrafficLight):TrafficLight =
    tl match {
      case Red => Green
      case Green => Yellow
      case Yellow => Red
    }
}
final case object Red extends TrafficLight
final case object Green extends TrafficLight
final case object Yellow extends TrafficLight

//////////

sealed trait Calculation
final case class Success(result:Int) extends Calculation
final case class Failure(reason:String) extends Calculation

object Calculator {
  def +(calc:Calculation, num:Int):Calculation =
    calc match {
      case Success(x) => Success(x + num)
      case Failure(x) => Failure(x)
    }
  def -(calc:Calculation, num:Int):Calculation =
    calc match {
      case Success(x) => Success(x - num)
      case Failure(x) => Failure(x)
    }
  def /(calc:Calculation, num:Int):Calculation =
    calc match {
      case _ => calc match {
        case Success(x) => num match {
          case 0 => Failure("Division by zero")
          case _ => Success (x / num)
        }
        case Failure(x) => Failure(x)
      }
    }

}


assert(Calculator.+(Success(1), 1) == Success(2))
assert(Calculator.-(Success(1), 1) == Success(0))
assert(Calculator.-(Failure("Badness"), 1) == Failure("Badness"))

assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
assert(Calculator./(Success(4), 2) == Success(2))
assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))

//////

case class Anonymous(id:String, createdAt:Date = new Date()) extends Visitor
case class User(
 id:String,
 email:String,
 createdAt:Date = new Date()

               ) extends Visitor

trait Visitor {
  def id:String
  def createdAt:Date
  def age:Long = new Date().getTime - createdAt.getTime
}

object EmailService {
  def apply(visitor:Visitor) =
    visitor match {
      case User(_, _, _) => "send email"
      case _ => "cannot send email"
    }
}





