object Five_3_4_Ex {
  sealed trait Tree[A] {
    def fold[B](node:(B,B) => B, leaf:A => B):B
  }

  final case class Node[A](left:Tree[A], right:Tree[A]) extends Tree[A] {
    def fold[B](node: (B,B) => B, leaf:A => B):B =
      node(left.fold(node, leaf), right.fold(node,leaf))
  }
  final case class Leaf[A](value: A) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B) =
      leaf(value)
  }


  val tree: Tree[String] =
    Node(Node(Leaf("To"), Leaf("iterate")),
      Node(Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"), Node(Leaf("recurse"), Leaf("diving")))))

  tree.fold[String]((a, b) => a + " " + b, str => str)

  sealed trait IntList {
    def fold[A](end:A, f:(Int, A) => A): A =
      this match {
        case End => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }
    def length:Int = fold[Int](0, (_, tl) => 1 + tl)
    def product:Int = fold[Int](1, (hd, tl) => hd * tl)
    def sum:Int = fold[Int](0, (hd, tl) => hd + tl)
    def double:IntList = fold[IntList](End, (hd, tl) => Pair(hd*2, tl))

  }

  final case object End extends IntList
  final case class Pair(head:Int, tail:IntList) extends IntList
  val example = Pair(2, Pair(4, Pair(6, End)))
  example.double
  example.sum
  example.product
  example.length
}