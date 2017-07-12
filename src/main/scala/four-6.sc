import scala.annotation.tailrec

sealed trait IntList
final case object End extends IntList
final case class Pair(head:Int, tail:IntList) extends IntList

val d = End
val c = Pair(3, d)
val b = Pair(2, c)
val a = Pair(1, b)

val example = Pair(1, Pair(2, Pair(3, End)))

@tailrec
def sum(list:IntList, total:Int = 0):Int =
  list match {
    case End => 0
    case Pair(hd, t) => sum(t, total + hd)
  }

assert(sum(example) == 6)
assert(sum(example.tail) == 5)
assert(sum(End) == 0)



