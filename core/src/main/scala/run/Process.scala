package typed
package run

sealed abstract class Process[-I, +O, +E, +A]
object Process {
  final case class Input[-I, +O, +E, +A](cont: I => Process[I, O, E, A])
      extends Process[I, O, E, A]

  final case class Output[-I, +O, +E, +A](
      output: O,
      cont: () => Process[I, O, E, A]
  ) extends Process[I, O, E, A]

  final case class Error[+E](error: E) extends Process[Any, Nothing, E, Nothing]

  final case class Result[+A](result: A)
      extends Process[Any, Nothing, Nothing, A]
}
