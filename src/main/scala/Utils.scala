object Utils {
  def chopCard(s: String): Option[(String, String)] = {
    try {
      val card = s.substring(0, 2)
      val rest = s.substring(2)
      Some(card, rest)
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }

  def traverse[A](list: Seq[Option[A]]): Option[Seq[A]] =
    list.foldLeft(Option(List[A]())) {
      (acc, opt) =>
        if (opt.isDefined)
          acc.flatMap(lst => opt.map(card => card :: lst))
        else
          None
    }

  def checkEmptyTail(tail: String): Option[Boolean] =
    if (tail.isEmpty) Some(true) else None
}
