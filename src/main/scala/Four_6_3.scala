import scala.annotation.tailrec

/*  / sfujimoto: 2017/07/11 6:53 */

object Four_6_3 extends App {
  sealed trait IntList {
    def length:Int =
      this match {
        case End => 0
        case Pair(hd, tl) => 1 + tl.length
      }

    def product:Int =
      this match {
        case End => 1
        case Pair(hd, tl) => hd * tl.product
      }

    def double:IntList =
      this match {
        case End => End
        case Pair(hd, tl) => Pair(hd * 2, tl.double)
      }
  }
  final case object End extends IntList
  final case class Pair(head:Int, tail:IntList) extends IntList

  val example = Pair(1, Pair(2, Pair(3, End)))

  @tailrec
  def sum(list:IntList, total:Int = 0):Int =
    list match {
      case End => 0
      case Pair(hd, t) => sum(t, total + hd)
    }



//  assert(sum(example) == 6, "for six")
//  assert(sum(example.tail) == 5, "for five")
//  assert(sum(End) == 0, "for End")
  assert(example.length == 3, "check length3")
  assert(example.tail.length == 2)
  assert(End.length == 0)

  assert(example.product == 6)
  assert(example.tail.product == 6)
  assert(End.product == 1)

  assert(example.double == Pair(2, Pair(4, Pair(6, End))))
  assert(example.tail.double == Pair(4, Pair(6, End)))
  assert(End.double == End)


  sealed trait Tree
  final case class Node(
                       left:Tree,
                       right:Tree
                       ) extends Tree
  final case class Leaf(elt:Int) extends Tree

  object TreeOps {
    def sum(tree:Tree):Int =
      tree match {
        case Leaf(elt) => elt
        case Node(l, r) => sum(l) + sum(r)
      }

    def double(tree:Tree):Tree =
      tree match {
        case Leaf(elt) => Leaf(elt * 2)
        case Node(l,r) => Node(double(l), double(r))
      }
  }

  val tree1 = Node(Leaf(2), Leaf(3))

  assert(TreeOps.sum(tree1) == 5)
  assert(TreeOps.double(tree1) == Node(Leaf(4), Leaf(6)))
}

