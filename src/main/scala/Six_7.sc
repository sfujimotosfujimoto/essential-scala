object Six_7 {
  for(x <- Seq(-2, -1, 0, 1, 2) if x > 0) yield x

  Seq(1, 2, 3) zip Seq(4, 5, 6)

  for(x <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield {
    val (a, b) = x; a + b
  }
  // instead
  for((a, b) <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield a + b


  for(x <- Seq(1, 2, 3).zipWithIndex) yield  x

  for {
    x <- Seq(1, 2, 3)
    square = x * x
    y <- Seq(4, 5, 6)
  } yield square * y

  





}