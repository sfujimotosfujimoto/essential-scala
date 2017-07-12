

object Five_2 {
  sealed trait IntList {
    def fold[A](end:A, f:(Int, A) => A): A =
      this match {
        case End => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }
    def length:Int =
      fold[Int](0, (_, tl) => 1 + tl)
    def product:Int =
      fold[Int](1, (hd, tl) => hd * tl)
    def sum:Int =
      fold[Int](0, (hd, tl) => hd + tl)
    def double: IntList =
      fold[IntList](End, (hd, tl) => Pair(hd * 2, tl))
  }
  final case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, Pair(4, Pair(5, End)))))

  example.length
  example.product
  example.sum
  example.double

  object Five_2_1 {
    sealed trait LinkedList[A] {
      def fold[B](end: B, pair: (A, B) => B): B =
        this match {
          case End() => end
          case Pair(hd, tl) => pair(hd, tl.fold(end, pair))

        }
    }
    final case class Pair[A](head:A, tail:LinkedList[A]) extends LinkedList[A]
    final case class End[A]() extends LinkedList[A]


    val p:LinkedList[String] =
      Pair("Hello", Pair("are", Pair("you", Pair("there", End()))))

    p.fold[String]("", (a, b) => a + " " + b)
  }

}



object Five_3_4_Ex {
  sealed trait Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B):B
  }
  final case class Node[A](left:Tree[A], right:Tree[A]) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B):B =
      node(left.fold(node,leaf), right.fold(node,leaf))
  }
  final case class Leaf[A](value:A) extends Tree[A] {
    def fold[B](node:(B,B) => B, leaf: A => B):B =
      leaf(value)
  }

  val tree: Tree[String] =
    Node(Node(Leaf("To"), Leaf("iterate")),
      Node(Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"), Node(Leaf("recurse"), Leaf("diving")))))


  val t1 = tree.fold[String]((a, b) => a + " " + b, str => str)

}
