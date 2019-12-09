object InputParser {
  // perform main input checks only without trying to convert to any types
  def checkInputStructure(input: String): Option[(String, Seq[String])] = {
    val parts = input split " "

    // input string should contain border and at least one hand
    def checkParts(): Option[Array[String]] =
      if (parts.length > 1) Some(parts) else None

    for {
      Array(b, tail @ _*) <- checkParts()
    } yield (b, tail)
  }
}
