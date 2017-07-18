object Five_6_Ex {
  sealed trait Sum[+A, +B] {
    def fold[C](error: A => C, success: B => C): C =
      this match {
        case Failure(v) => error(v)
        case Success(v) => success(v)
      }

    def map[C](fn: B => C): Sum[A, C] =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => Success(fn(v))
      }

    def flatMap[AA >: A, C](f:B => Sum[AA, C]): Sum[AA, C] =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => f(v)
      }

  }
  final case class Failure[A](value:A) extends Sum[A, Nothing]
  final case class Success[B](value:B) extends Sum[Nothing, B]


  /////
  sealed trait Expression {
    def eval: Sum[String, Double] =
      this match {
        case Addition(l, r) => lift2(l, r, (left, right) => Success(left + right))
        case Subtraction(l, r) => lift2(l, r, (left, right) => Success(left - right))
        case Division(l, r) => lift2(l, r,
          (left, right) =>
            if (right == 0)
              Failure("Division by zero")
            else
              Success(left / right)
        )
        case SquareRoot(v) =>
          v.eval flatMap { value =>
            if (value < 0)
              Failure("Square root of negative number")
            else
              Success(Math.sqrt(value))
          }
        case Number(v) => Success(v)

      }

    def lift2(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]) =
      l.eval flatMap {
        left => r.eval flatMap {
          right => f(left, right)
        }
      }
  }
  final case class Addition(left: Expression, right: Expression) extends Expression
  final case class Subtraction(left: Expression, right: Expression) extends Expression
  final case class Division(left: Expression, right: Expression) extends Expression
  final case class SquareRoot(value: Expression) extends Expression
  final case class Number(value: Double) extends Expression

  assert(Addition(Number(1), Number(2)).eval == Success(3))
  assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))





  ////



  case class Box[+A](value: A) {
    def set[AA >: A](a: AA): Box[AA] = Box(a)

  }

}