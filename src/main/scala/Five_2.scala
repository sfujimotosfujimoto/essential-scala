object Five_3_4_Ex extends App {
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
  println(t1)

}

object Five_2_1 extends App {
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

  println(p.fold[String]("", (a, b) => a + " " + b))
}
