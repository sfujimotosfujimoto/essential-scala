class Person(val firstName:String, val lastName:String)
object Person {
  def apply(name:String): Person = {
    val parts = name.split(" ")
    new Person(parts(0), parts(1))
  }
}
Person("Don John").firstName



class Timestamp(val seconds:Long)
object Timestamp {
  def apply(hours:Int, minutes:Int, seconds:Int):Timestamp =
    new Timestamp(hours*60*60 + minutes*60 + seconds)
}

Timestamp(1, 1, 1).seconds




class Counter(val count:Int) {
  def inc:Counter = inc()
  def dec:Counter = dec()
  def inc(amount:Int = 1) = new Counter(count + amount)
  def dec(amount:Int = 1) = new Counter(count - amount)

  def adjust(adder:Adder):Counter = new Counter(adder(count))
}

new Counter(10).inc().adjust(new Adder(12)).count
new Counter(12).dec().count

class Adder(amount:Int) {
  def apply(in:Int):Int = in + amount
}

val add3 = new Adder(3)
add3(2)





class Cat(val name:String, val color:String, val food:String) {
  override def toString: String = s"$name is $color and eats $food"

}

val c1 = new Cat("Oswald", "Black", "Milk")
val c2 = new Cat("Henderson", "Ginger", "Chips")
val c3 = new Cat("Quentin", "Tabby and white", "Curry")

object ChipShop {
  def willServe(cat:Cat) =
    if (cat.food.toLowerCase == "chips") true else false
}

ChipShop.willServe(c1)
ChipShop.willServe(c2)

class Director(
                val firstName:String,
                val lastName:String,
                val yearOfBirth:Int) {
  def name = s"$firstName $lastName"

  def copy(
          firstName:String = this.firstName,
          lastName:String = this.lastName,
          yearOfBirth:Int = this.yearOfBirth
          ) =
    new Director(firstName, lastName, yearOfBirth)

  override def toString = s"Director($firstName, $lastName, $yearOfBirth)"
}

object Director {
  def apply(firstName:String, lastName:String, yearOfBirth:Int):Director =
    new Director(firstName, lastName, yearOfBirth)

  def older(dir1:Director, dir2:Director) =
    if (dir1.yearOfBirth > dir2.yearOfBirth) dir2 else dir1
}





class Film(
                 val name:String,
                 val yearOfRelease:Int,
                 val imdbRating:Double,
                 val director:Director
               ) {
  def directorsAge =
    yearOfRelease - director.yearOfBirth

  def isDirectedBy(dir:Director):Boolean =
    if (this.director == dir) true else false

  def copy(
            name:String = this.name,
            yearOfRelease:Int = this.yearOfRelease,
            imdbRating:Double = this.imdbRating,
            director:Director = this.director) =
    new Film(name, yearOfRelease, imdbRating, director)
  override def toString = s"Film($name, $yearOfRelease, $imdbRating, $director)"
}


object Film {
  def apply(
             name:String,
             yearOfRelease:Int,
             imdbRating:Double, director:Director):Film =
    new Film(name, yearOfRelease, imdbRating, director)

  def highestRating(film1:Film, film2:Film):Double =
    if(film1.imdbRating > film2.imdbRating) film1.imdbRating else film2.imdbRating
  def oldestDirectorAtTheTime(film1:Film, film2:Film):Director =
    if (film1.directorsAge > film2.directorsAge) film1.director else film2.director
}



val eastwood  = new Director("Clint", "Eastwood", 1930)
val mcTiernan = new Director("John", "McTiernan", 1951)
val nolan  = new Director("Christopher", "Nolan", 1970)
val someBody   = new Director("Just", "Some Body", 1990)

val memento  = new Film("Memento", 2000, 8.5, nolan)
val darkKnight  = new Film("Dark Knight", 2008, 9.0, nolan)
val inception  = new Film("Inception", 2010, 8.8, nolan)
val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
val outlawJoseyWales  = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
val unforgiven  = new Film("Unforgiven", 1992, 8.3, eastwood)
val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
val invictus  = new Film("Invictus", 2009, 7.4, eastwood)
val predator  = new Film("Predator", 1987, 7.9, mcTiernan)
val dieHard  = new Film("Die Hard", 1988, 8.3, mcTiernan)
val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

eastwood.yearOfBirth
dieHard.director.name
invictus.isDirectedBy(eastwood)

highPlainsDrifter.copy(name = "L'homme des hautes plaines")

Film.highestRating(unforgiven, granTorino)
Film.oldestDirectorAtTheTime(unforgiven, granTorino)
Director.older(eastwood, nolan)


case class Dummy(name:String)
val dave = new Dummy("Daveie")
dave.copy() eq dave


