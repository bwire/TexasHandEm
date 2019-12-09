object TaxesHandEm {
  def main(args: Array[String]): Unit = {
    // TODO replace with readLine
    val input = "4cKs4h8s7s 5 Ad4s Ac4d As9s KhKd 5d6d"
    val result = for {
      (b, n, h) <- InputParser.checkInputStructure(input)
      game <- Game(b, n, h)
      result <- game.yieldResult()
    } yield result

    println(result.getOrElse("Incorrect input"))
  }
}
