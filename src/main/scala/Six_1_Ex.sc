object Six_1_Ex {
  val seq = Seq(1, 2, 3, 4, 5)
  seq.length
  seq.size
  seq.mkString(",")
  seq.mkString("[ ", ", ", " ]")
  Some(123).isDefined

  //// 6.1.9.2 Animals
  val animals = Seq("cat", "dog", "penguin")
  animals :+ "tyrannosaurus"
  "mouse" +: animals
  2 +: animals

  //// 6.1.9.3 Movie

  case class Film(
                 name: String,
                 yearOfRelease: Int,
                 imdbRating: Double
                 )
  case class Director(
                     firstName: String,
                     lastName: String,
                     yearOfBirth: Int,
                     films: Seq[Film]
                     )
  val memento           = new Film("Memento", 2000, 8.5)
  val darkKnight        = new Film("Dark Knight", 2008, 9.0)
  val inception         = new Film("Inception", 2010, 8.8)

  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales  = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven        = new Film("Unforgiven", 1992, 8.3)
  val granTorino        = new Film("Gran Torino", 2008, 8.2)
  val invictus          = new Film("Invictus", 2009, 7.4)

  val predator          = new Film("Predator", 1987, 7.9)
  val dieHard           = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = new Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  val someGuy = new Director("Just", "Some Guy", 1990, Seq())
  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  def directorsWithBackCatalogOfSize(num: Int): Seq[Director] = {
    directors.filter(_.films.length > num)
  }
  directorsWithBackCatalogOfSize(3)

  def directorsBornBefore(year: Int): Seq[Director] =
    directors.filter(_.yearOfBirth < year)

  directorsBornBefore(1970).size

  def directorsBornBeforeAndBackCatologSize(numberOfFilms: Int, year: Int): Seq[Director] = {
    val byAge = directors.filter(_.yearOfBirth < year)
    val byFilms = directors.filter(_.films.length > numberOfFilms)
    byAge.filter(byFilms.contains)

  }

  directorsBornBeforeAndBackCatologSize(4, 1970).size

  def directorsSortedByAge(ascending: Boolean = true) = {
      if (ascending) {
        directors.sortWith((a, b) => a.yearOfBirth < b.yearOfBirth)
      } else {
        directors.sortWith((a, b) => a.yearOfBirth > b.yearOfBirth)
      }
  }







}