
object divide {
  def apply(a:Int, b:Int):DivisionResult =
    if(b == 0) Infinite else Finite(a / b)
}


sealed trait DivisionResult
final case class Finite(value:Int) extends DivisionResult
final case object Infinite extends DivisionResult

divide(1, 0) match {
  case Finite(value) => s"It's finite: ${value}"
  case Infinite => s"It's infinite"
}

def division(a:Int, b:Int) = {
  divide(a, b) match {
    case Finite(value) => s"It's finite: ${value}"
    case Infinite => s"It's infinite"
  }
}

division(2, 3)
division(12, 0)
division(5, 3)
