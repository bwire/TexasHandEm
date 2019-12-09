object TexasHandEm {
  def main(args: Array[String]): Unit = {
    while (true) {
      val input = scala.io.StdIn.readLine()
      val result = for {
        (b, h) <- InputParser.checkInputStructure(input)
        game <- Game(b, h)
        result <- game.yieldResult()
      } yield result

      println(result.getOrElse("Incorrect input"))
    }
  }
}