sealed trait Shape {
  def sides:Int
  def perimeter:Double
  def area:Double

}


trait Rectangular extends Shape {
  def width:Int
  def height:Int
  val sides = 4
  val perimeter = 2 * width + 2 * height
  val area = width * height
}

case class Circle(radius:Int) extends Shape {
  val sides = 1
  def perimeter = 2 * math.Pi * radius
  def area = radius * radius * math.Pi
}

case class Rectangle(width:Int, height:Int) extends Rectangular {
}

case class Square(side:Int) extends Rectangular {
  override def width: Int = side
  override def height: Int = side
}



object Draw {
  def apply(shape:Shape):String =
    shape match {
      case Circle(x) => s"A circle of radius $x"
      case Rectangle(x, y) => s"A rectangle of width ${x}cm and height ${y}cm"
      case Square(x) => s"A square of side ${x}cm"
    }
}