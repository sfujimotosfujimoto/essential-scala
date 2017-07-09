import java.util.Date

sealed trait Visitor {
  def id:String
  def createdAt:Date
  def age:Long = new Date().getTime() - createdAt.getTime()

}

final case class Anonymous(id:String, createdAt:Date = new Date())
  extends Visitor

final case class User(
                 id:String,
                 email:String,
                 createdAt:Date = new Date()
               ) extends Visitor



sealed trait Color {
  def red:Int
  def green:Int
  def blue:Int

  def isLight = (red + green + blue) / 3.0 > 127
  def isDark = !isLight
}

final case object Red extends Color{
  val red = 255
  val green = 0
  val blue = 0


}

final case object Yellow extends Color {
  val red = 255
  val green = 255
  val blue = 0


}

final case object Pink extends Color {
  val red = 255
  val green = 0
  val blue = 255
}

final case class CustomColor(
                            red:Int,
                            green:Int,
                            blue:Int
                            ) extends Color


trait Shape {
  def sides:Int
  def perimeter:Double
  def area:Double

  def color:Color
}


trait Rectangular extends Shape {
  def width:Int
  def height:Int
  val sides = 4
  val perimeter = 2 * width + 2 * height
  val area = width * height
}

case class Circle(radius:Int, color:Color) extends Shape {
  val sides = 1
  def perimeter = 2 * math.Pi * radius
  def area = radius * radius * math.Pi

}

case class Rectangle(width:Int, height:Int, color:Color) extends Rectangular {
}

case class Square(side:Int, color:Color) extends Rectangular {
  override def width: Int = side
  override def height: Int = side
}

object Draw {
  def apply(shape:Shape):String =
    shape match {
      case Circle(x, color) => s"A ${Draw(color)} circle of radius $x"
      case Rectangle(x, y, color) => s"A ${Draw(color)} rectangle of width ${x}cm and height ${y}cm"
      case Square(x, color) => s"A ${Draw(color)} square of side ${x}cm"
    }

  def apply(color:Color):String =
    color match {
      case Red => "red"
      case Yellow => "yellow"
      case Pink => "pink"
      case color => if(color.isLight) "light" else "dark"
    }
}

val c = Circle(7, Yellow)
Draw(c)
Draw(Rectangle(3, 4, CustomColor(128, 128, 101)))