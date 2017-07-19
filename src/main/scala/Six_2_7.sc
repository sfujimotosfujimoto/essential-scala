object Six_2_7 {
  // 6.2.7.2 Do It Yourself
  // Minimum
  def minimum(seq: Seq[Int]): Int =
  seq.foldLeft(Int.MaxValue)((a, b) => if (a < b) a else b)

  val seq = Seq(34, 12, 45, 123, 34)
  minimum(seq)

  // Unique
  def insert(seq: Seq[Int], elt: Int): Seq[Int] = {
    if(seq.contains(elt))
      seq
    else
      elt +: seq
  }
  def unique(seq: Seq[Int]): Seq[Int] =
    seq.foldLeft(Seq.empty[Int]){ (a, b) => insert(a, b)}

  unique(Seq(1, 1, 2, 2, 3, 3, 4, 5, 5 ))

  seq.foldLeft(1) { (a, b) => a * b}

  // Reverse
  def reverse[A](seq: Seq[A]): Seq[A] =
    seq.foldLeft(Seq.empty[A]){(seq, elt) => elt +: seq}

  reverse(seq)


  // Map
  def map[A, B](seq: Seq[A], f: A => B): Seq[B] =
    seq.foldRight(Seq.empty[B]){(elt, seq) => f(elt) +: seq}

  // Fold Left
  def foldLeft[A, B](seq: Seq[A], zero: B, f: (B, A) => B): B = {
    var result: B = zero
    seq.foreach {(elt: A) => result = f(result, elt) }
    result
  }


}