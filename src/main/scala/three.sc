
case class Cat(name:String, color:String, food:String)


case class Director(
                val firstName:String,
                val lastName:String,
                val yearOfBirth:Int) {
  def name = s"$firstName $lastName"
}

object Director {
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

}


object Film {
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




//////////////


case class Counter(val count:Int = 0) {
  def inc:Counter = copy(count = count + 1)
  def dec:Counter = copy(count = count - 1)

  def adjust(adder:Adder):Counter = Counter(adder(count))
}



Counter(10).inc.adjust(new Adder(12)).count
Counter(12).dec.count

class Adder(amount:Int) {
  def apply(in:Int):Int = in + amount
}

val add3 = new Adder(3)
add3(2)

