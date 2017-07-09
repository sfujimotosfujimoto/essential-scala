import java.util.Date



////////////

trait Feline {
  def color:String
  def sound:String
}

case class Tiger(color:String) extends Feline {
  val sound = "roar"
}

case class Lion(color:String, maneSize:Int) extends Feline {
  val sound = "roar"
}

case class Panther(color:String) extends Feline {
  val sound = "roar"
}

case class Cat(color:String, food:String) extends Feline {
  val sound = "meow"
}


//////
trait Shape {
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

Draw(Circle(10))
Draw(Square(10))
Draw(Rectangle(10, 5))



val c = Circle(12)
val r = Rectangle(4, 5)
val s = Square(6)

c.area
c.perimeter
c.sides

r.area
r.perimeter
r.sides

s.area
s.perimeter
s.sides




///////
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

def older(v1:Visitor, v2:Visitor):Boolean =
  v1.createdAt.before(v2.createdAt)

older(Anonymous("1"), User("2", "test@example.com"))

val anon1 = Anonymous("anon1")
anon1.createdAt
anon1.age

