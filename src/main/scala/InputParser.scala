object InputParser {
  // perform main input checks only without trying to convert to any types
  def checkInputStructure(input: String): Option[(String, Int, Seq[String])] = {
    val parts = input split " "

    // input string should conatain border, number of hands and at least one hand
    def checkParts(): Option[Array[String]] =
      if (parts.length > 2) Some(parts) else None

    def toInt(s: String): Option[Int] = {
      try {
        Some(s.toInt)
      } catch {
        case _: NumberFormatException => None
      }
    }

    for {
      Array(b, n, tail @ _*) <- checkParts()
      numHands <- toInt(n)
    } yield (b, numHands, tail)
  }
}
