package typed

/** An uninhabited type */
final abstract class False {
  def fold[A]: A
}
