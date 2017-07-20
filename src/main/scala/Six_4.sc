object Six_4 {
  def readInt(str: String): Option[Int] =
    if(str matches "\\d+") Some(str.toInt) else None
  readInt("123")
  readInt("abc3")

  readInt("abc").getOrElse(0)

  readInt("123") match {
    case Some(number) => number + 1
    case None => 0
  }
  def sum(opA: Option[Int], opB: Option[Int]): Option[Int] =
    opA.flatMap(a => opB.map(b => a + b))

  sum(readInt("1"), readInt("34"))

  //// 6.5.1 Ex
  def addOptions(opA: Option[Int], opB: Option[Int]): Option[Int] = {
    for {
      a <- opA
      b <- opB
    } yield a + b

  }

  def addOptions(opA: Option[Int], opB: Option[Int], opC: Option[Int]) =
    for {
      a <- opA
      b <- opB
      c <- opC
    } yield a + b + c



  addOptions(Some(3), Some(23), Some(100))


  def addOptions2(opA: Option[Int], opB: Option[Int]): Option[Int] =
    opA.flatMap(a => opB.map(b => a + b))

  def addOptions2(opA: Option[Int], opB: Option[Int], opC: Option[Int]) =
    opA flatMap { a =>
        opB flatMap { b =>
            opC.map { c =>
              a + b + c
            }
        }
    }

  addOptions2(Some(23), Some(12), Some(999))

  def divide(a: Int, b: Int): Option[Int] =
    if (b < 1) None else Some(a / b)

  /////////////////
  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    val result = for {
      a <- readInt(operand1)
      b <- readInt(operand2)
      ans <- operator match {
        case "+" => Some(a + b)
        case "-" => Some(a - b)
        case "*" => Some (a * b)
        case "/" => divide(a, b)
        case _  => None
      }
    } yield ans

    result match {
      case Some(number) => println(s"The answer is $number!")
      case None => println(s"Error calculating $operand1 $operator $operand2")
    }
  }

  calculator("123", "*", "3")
  





}