object Six_8 {
  val example = Map("a" -> 1, "b" -> 2, "c" -> 3)

  example.map(pair => pair._1 -> pair._2 * 2)

  example.map(pair => pair._1 + " = " + pair._2)

  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")

  val ages = Map(
    "Alice"   -> 20,
    "Bob"     -> 30,
    "Charlie" -> 50,
    "Derek"   -> 40,
    "Edith"   -> 10,
    "Fred"    -> 60)

  val favoriteColors = Map(
    "Bob"     -> "green",
    "Derek"   -> "magenta",
    "Fred"    -> "yellow")

  val favoriteLolcats = Map(
    "Alice"   -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith"   -> "Cloud Cat")

  def favoriteColor(name: String):String =
    favoriteColors.get(name).getOrElse("beige")

  def printColors:Unit = {
    for {
      person <- people
    } println(s"${person}'s favorite color is ${favoriteColor(person)}!")

  }
  printColors

  def lookup[A](name:String, values:Map[String, A]) ={
    values.get(name)
  }

  lookup("Alice", favoriteLolcats)


  favoriteColor("Bob")

  val oldest: Option[String] =
    people.foldLeft(Option.empty[String]) {
      (older, person) =>
        if(ages.get(person).getOrElse(0) > older.flatMap(ages.get).getOrElse(0)) {
          Some(person)
        } else {
          older
        }
    }

  val favorite: Option[String] =
    for {
      oldest <- oldest
      color <- favoriteColors.get(oldest)
    } yield color



}