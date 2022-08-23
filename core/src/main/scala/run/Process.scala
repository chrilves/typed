package typed
package run

enum Process[-I, +O, +E, +A]:
  case Input[-I, +O, +E, +A](cont: I => Process[I, O, E, A]) extends Process[I, O, E, A]
  case Output[-I, +O, +E, +A](
      output: O,
      cont: () => Process[I, O, E, A]
  )                          extends Process[I, O, E, A]
  case Error[+E](error: E)   extends Process[Any, Nothing, E, Nothing]
  case Result[+A](result: A) extends Process[Any, Nothing, Nothing, A]
