package typed
package run

import typed.run.IO.Instrumentation

trait Semigroup[A] {
  def combine(a1: A, a2: A): A
}
object Semigroup {
  @inline def apply[A](implicit ev: Semigroup[A]): ev.type = ev
}

trait Monoid[A] extends Semigroup[A] {
  def zero: A
}
object Monoid {
  @inline def apply[A](implicit ev: Monoid[A]): ev.type = ev
}

sealed abstract class IO[-R, +W, +T, +E, +A] {
  @inline final def mapRead[R2](f: R2 => R): IO[R2, W, T, E, A] =
    IO.mapRead(this)(f)

  @inline final def mapWrite[W2](f: W => W2): IO[R, W2, T, E, A] =
    IO.mapWrite(this)(f)

  @inline final def mapTag[T2](f: T => T2): IO[R, W, T2, E, A] =
    IO.mapTag(this)(f)

  @inline final def mapError[E2](f: E => E2): IO[R, W, T, E2, A] =
    IO.mapError[R, W, T, E, E2, A](this)(f)

  @inline final def map[A2](f: A => A2): IO[R, W, T, E, A2] =
    IO.map(this)(f)

  @inline final def tag[T2 >: T](t: T2): IO[R, W, T2, E, A] =
    IO.tag(t)(this)

  @inline final def instrument[R2 <: R, W2 >: W, T2](
      instrumentation: Instrumentation[R2, W2, T, T2]
  ): IO[R2, W2, T2, E, A] =
    IO.instrument[R2, W2, T, T2](instrumentation)(this)

  @inline final def flatMap[R2 <: R, W2 >: W, T2 >: T, E2 >: E, B](
      f: A => IO[R2, W2, T2, E2, B]
  ): IO[R2, W2, T2, E2, B] =
    IO.flatMap[R2, W2, T2, E2, A, B](this)(f)

  @inline final def ap[R2 <: R, W2 >: W, T2 >: T, E2 >: E, B](
      f: IO[R2, W2, T2, E2, A => B]
  ): IO[R2, W2, T2, E2, B] =
    IO.ap(f, this)

  @inline final def toProcess[E2 >: E](
      implicit E2: Semigroup[E2]
  ): Process[R, W, E2, A] =
    IO.toProcess[R, W, T, E2, A](this)

  @inline final def run[W2 >: W, E2 >: E](
      r: R
  )(
      implicit W2: Semigroup[W2],
      E2: Semigroup[E2]
  ): (Option[W2], Either[E2, A]) =
    IO.run[R, W2, T, E2, A](this)(r)
}

object IO {

  /** Reader operation: get the input value of type R */
  private final case class Read[R]() extends IO[R, Nothing, Nothing, Nothing, R]

  @inline def read[R]: IO[R, Nothing, Nothing, Nothing, R] =
    Read[R]()

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  @inline def mapRead[R1, R2, W, T, E, A](
      mainIO: IO[R1, W, T, E, A]
  )(f: R2 => R1): IO[R2, W, T, E, A] = {

    def aux[E0, A0](io: IO[R1, W, T, E0, A0]): IO[R2, W, T, E0, A0] =
      io match {
        case Pure(a)      => Pure(a)
        case Error(e)     => Error(e)
        case _: Read[r]   => map(Read[R2]())(f)
        case Write(w)     => Write(w)
        case Tag(t, i)    => Tag(t, aux(i))
        case Defer(d)     => Defer(() => aux(d()))
        case Ap(fun, arg) => Ap(aux(fun), aux(arg))
        case Flatten(v)   => flatMap(aux(v))(aux(_))
        case c @ CatchError(i, h) =>
          CatchError(aux(i), h.andThen(aux(_)))(c.errorSemiGroupInstance)
      }

    aux(mainIO)
  }

  /** Writer operation: output some value of type W */
  private final case class Write[+W](value: W)
      extends IO[Any, W, Nothing, Nothing, Unit]

  @inline def write[W](value: W): IO[Any, W, Nothing, Nothing, Unit] =
    Write[W](value)

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  @inline def mapWrite[R, W1, W2, T, E, A](
      mainIO: IO[R, W1, T, E, A]
  )(f: W1 => W2): IO[R, W2, T, E, A] = {

    def aux[E0, A0](io: IO[R, W1, T, E0, A0]): IO[R, W2, T, E0, A0] =
      io match {
        case Pure(a)      => Pure(a)
        case Error(e)     => Error(e)
        case _: Read[r]   => Read()
        case Write(w)     => Write(f(w))
        case Tag(t, i)    => Tag(t, aux(i))
        case Defer(d)     => Defer(() => aux(d()))
        case Ap(fun, arg) => Ap(aux(fun), aux(arg))
        case Flatten(v)   => flatMap(aux(v))(aux(_))
        case c @ CatchError(i, h) =>
          CatchError(aux(i), h.andThen(aux(_)))(c.errorSemiGroupInstance)
      }

    aux(mainIO)
  }

  /** Tag operation: tag some computation with tag of type T */
  private final case class Tag[-R, +W, +T, +E, +A](
      tag: T,
      io: IO[R, W, T, E, A]
  ) extends IO[R, W, T, E, A]

  @inline def tag[R, W, T, E, A](
      tag: T
  )(io: IO[R, W, T, E, A]): IO[R, W, T, E, A] =
    Tag(tag, io)

  trait TagTransformation[R, W, -T1, +T2] {
    def apply[E, A](io: IO[R, W, T1, E, A]): IO[R, W, T2, E, A]
  }
  object TagTransformation {
    def id[R, W, T]: TagTransformation[R, W, T, T] =
      new TagTransformation[R, W, T, T] {
        def apply[E, A](io: IO[R, W, T, E, A]): IO[R, W, T, E, A] = io
      }
  }

  trait Instrumentation[R, W, -T1, +T2] { self =>
    def apply[T](tag: T1)(
        f: TagTransformation[R, W, T2, T]
    ): TagTransformation[R, W, T, T]
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  @inline def instrument[R, W, T1, T2](
      instrumentation: Instrumentation[R, W, T1, T2]
  ): TagTransformation[R, W, T1, T2] =
    new TagTransformation[R, W, T1, T2] { self =>
      def apply[E0, A0](io: IO[R, W, T1, E0, A0]): IO[R, W, T2, E0, A0] =
        io match {
          case Pure(a)    => Pure(a)
          case Error(e)   => Error(e)
          case _: Read[r] => Read()
          case Write(w)   => Write(w)
          case Tag(t, i) =>
            instrumentation[T2](t)(TagTransformation.id)(apply(i))
          case Defer(d)     => Defer(() => apply(d()))
          case Ap(fun, arg) => Ap(apply(fun), apply(arg))
          case Flatten(v)   => flatMap(apply(v))(apply(_))
          case c @ CatchError(i, h) =>
            CatchError(apply(i), h.andThen(apply(_)))(c.errorSemiGroupInstance)
        }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  @inline def mapTag[R, W, T1, T2, E, A](
      mainIO: IO[R, W, T1, E, A]
  )(f: T1 => T2): IO[R, W, T2, E, A] = {
    def aux[E0, A0](io: IO[R, W, T1, E0, A0]): IO[R, W, T2, E0, A0] =
      io match {
        case Pure(a)      => Pure(a)
        case Error(e)     => Error(e)
        case _: Read[r]   => Read()
        case Write(w)     => Write(w)
        case Tag(t, i)    => Tag(f(t), aux(i))
        case Defer(d)     => Defer(() => aux(d()))
        case Ap(fun, arg) => Ap(aux(fun), aux(arg))
        case Flatten(v)   => flatMap(aux(v))(aux(_))
        case c @ CatchError(i, h) =>
          CatchError(aux(i), h.andThen(aux(_)))(c.errorSemiGroupInstance)
      }

    aux(mainIO)
  }

  /** Monad Error operation: fail with error of type E */
  private final case class Error[+E](error: E)
      extends IO[Any, Nothing, Nothing, E, Nothing]

  @inline def error[E](err: E): IO[Any, Nothing, Nothing, E, Nothing] =
    Error(err)

  /** Monad Error operation: catch errors of type E */
  private final case class CatchError[-R, +W, +T, E1, +E2, +A](
      io: IO[R, W, T, E1, A],
      handler: E1 => IO[R, W, T, E2, A]
  )(implicit val errorSemiGroupInstance: Semigroup[E1])
      extends IO[R, W, T, E2, A]

  @inline def catchError[R, W, T, E1: Semigroup, E2, A](
      io: IO[R, W, T, E1, A]
  )(handler: E1 => IO[R, W, T, E2, A]): IO[R, W, T, E2, A] =
    io match {
      case Pure(a)  => Pure(a)
      case Error(e) => handler(e)
      case _        => CatchError(io, handler)
    }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  @inline def mapError[R, W, T, E1, E2, A](
      mainIO: IO[R, W, T, E1, A]
  )(f: E1 => E2): IO[R, W, T, E2, A] = {

    def aux[A0](io: IO[R, W, T, E1, A0]): IO[R, W, T, E2, A0] =
      io match {
        case Pure(a)      => Pure(a)
        case Error(e)     => Error(f(e))
        case _: Read[r]   => Read()
        case Write(w)     => Write(w)
        case Tag(t, i)    => Tag(t, aux(i))
        case Defer(d)     => Defer(() => aux(d()))
        case Ap(fun, arg) => Ap(aux(fun), aux(arg))
        case Flatten(v)   => flatMap(aux(v))(aux(_))
        case c @ CatchError(i, h) =>
          CatchError(i, h.andThen(aux(_)))(c.errorSemiGroupInstance)
      }

    aux(mainIO)
  }

  /** Applicative operation Pure */
  private final case class Pure[+A](value: A)
      extends IO[Any, Nothing, Nothing, Nothing, A]

  @inline def pure[A](value: A): IO[Any, Nothing, Nothing, Nothing, A] =
    Pure(value)

  /** Applicative operation Ap */
  private final case class Ap[-R, +W, +T, +E, X, +A](
      fun: IO[R, W, T, E, X => A],
      arg: IO[R, W, T, E, X]
  ) extends IO[R, W, T, E, A]

  @inline def ap[R, W, T, E, X, A](
      fun: IO[R, W, T, E, X => A],
      arg: IO[R, W, T, E, X]
  ): IO[R, W, T, E, A] =
    (fun, arg) match {
      case (Error(f), Pure(_)) => Error(f)
      case (Pure(_), Error(a)) => Error(a)
      case (Pure(f), Pure(x))  => Pure(f(x))
      case (_, _)              => Ap(fun, arg)
    }

  /** Monad operation Flatten */
  private final case class Flatten[-R, +W, +T, +E, +A](
      value: IO[R, W, T, E, IO[R, W, T, E, A]]
  ) extends IO[R, W, T, E, A]

  @inline def flatten[R, W, T, E, A](
      value: IO[R, W, T, E, IO[R, W, T, E, A]]
  ): IO[R, W, T, E, A] =
    value match {
      case Error(e) => Error(e)
      case Pure(io) => io
      case _        => Flatten(value)
    }

  /** Defer operation */
  private final case class Defer[-R, +W, +T, +E, +A](
      deferred: () => IO[R, W, T, E, A]
  ) extends IO[R, W, T, E, A]

  @inline def defer[R, W, T, E, A](
      deferred: () => IO[R, W, T, E, A]
  ): IO[R, W, T, E, A] =
    Defer(deferred)

  @inline def lazyli[R, W, T, E, A](
      deferred: => IO[R, W, T, E, A]
  ): IO[R, W, T, E, A] =
    Defer(() => deferred)

  @inline def map[R, W, T, E, X, A](
      io: IO[R, W, T, E, X]
  )(f: X => A): IO[R, W, T, E, A] =
    ap(Pure(f), io)

  @inline def flatMap[R, W, T, E, X, A](
      io: IO[R, W, T, E, X]
  )(f: X => IO[R, W, T, E, A]): IO[R, W, T, E, A] =
    flatten(map(io)(f))

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def toProcess[R, W, T, E: Semigroup, A](
      mainIO: IO[R, W, T, E, A]
  ): Process[R, W, E, A] = {

    def aux[E0: Semigroup, A0](
        io: IO[R, W, T, E0, A0],
        k: A0 => Process[R, W, E, A],
        h: E0 => Process[R, W, E, A]
    ): Process[R, W, E, A] =
      io match {
        case Pure(a)    => k(a)
        case Error(e)   => h(e)
        case _: Read[R] => Process.Input(k)
        case Write(w)   => Process.Output(w, () => k(()))
        case Tag(_, i)  => aux(i, k, h)
        case Defer(d)   => aux(d(), k, h)
        case ap: Ap[R, W, T, E0, x, A0] =>
          aux(
            ap.fun,
            (f: x => A0) => aux(ap.arg, (x: x) => k(f(x)), h),
            (e: E0) =>
              aux(
                ap.arg,
                (_: x) => h(e),
                (e2: E0) => h(Semigroup[E0].combine(e, e2))
              )
          )
        case Flatten(v) =>
          aux[E0, IO[R, W, T, E0, A0]](
            v,
            (i: IO[R, W, T, E0, A0]) => aux(i, k, h),
            h
          )
        case ce: CatchError[R, W, T, e, E0, A0] =>
          aux[e, A0](ce.io, k, ce.handler.andThen(aux(_, k, h)))(
            ce.errorSemiGroupInstance
          )
      }

    aux[E, A](
      mainIO,
      Process.Result(_),
      Process.Error(_)
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def run[R, W: Semigroup, T, E: Semigroup, A](
      mainIO: IO[R, W, T, E, A]
  )(r: R): (Option[W], Either[E, A]) = {

    def mergeWrite(ow1: Option[W], ow2: Option[W]): Option[W] =
      (ow1, ow2) match {
        case (None, _) => ow2
        case (_, None) => ow1
        case (Some(w1), Some(w2)) =>
          Some(Semigroup[W].combine(w1, w2))
      }

    def aux[E0: Semigroup, A0](
        io: IO[R, W, T, E0, A0]
    ): (Option[W], Either[E0, A0]) =
      io match {
        case Pure(a)    => (None, Right(a))
        case Error(e)   => (None, Left(e))
        case _: Read[R] => (None, Right(r))
        case Write(w)   => (Some(w), Right(()))
        case Tag(_, i)  => aux(i)
        case Defer(d)   => aux(d())
        case ap: Ap[R, W, T, E0, x, A0] =>
          val (ow1, rf) = aux(ap.fun)
          val (ow2, ra) = aux(ap.arg)

          val ret: Either[E0, A0] =
            (rf, ra) match {
              case (Right(f), Right(a)) => Right(f(a))
              case (Left(x), Left(y))   => Left(Semigroup[E0].combine(x, y))
              case (Left(x), _)         => Left(x)
              case (_, Left(y))         => Left(y)
            }

          (mergeWrite(ow1, ow2), ret)

        case Flatten(v) =>
          val (ow1, ret1) = aux[E0, IO[R, W, T, E0, A0]](v)

          ret1 match {
            case Left(e) => (ow1, Left(e))
            case Right(io2) =>
              val (ow2, ret2) = aux(io2)
              (mergeWrite(ow1, ow2), ret2)
          }

        case ce: CatchError[R, W, T, e, E0, A0] =>
          val (ow1, ret1) =
            aux(ce.io)(ce.errorSemiGroupInstance)
          ret1 match {
            case Left(e1) =>
              val (ow2, ret2) = aux(ce.handler(e1))
              (mergeWrite(ow1, ow2), ret2)
            case Right(a) => (ow1, Right(a))
          }
      }

    aux[E, A](mainIO)
  }
}

sealed abstract class Ctx[+R1, -R2, -W1, +W2, -T1, +T2, -E1, +E2]

final case class NOOP[R, W, T, E]() extends Ctx[R, R, W, W, T, T, E, E]

final case class MapRead[+R1, R2, -R3, -W1, +W2, -T1, +T2, -E1, +E2](
    f: R3 => R2,
    ctx: Ctx[R1, R2, W1, W2, T1, T2, E1, E2]
) extends Ctx[R1, R3, W1, W2, T1, T2, E1, E2]

final case class MapWrite[+R1, -R2, -W1, W2, +W3, -T1, +T2, -E1, +E2](
    ctx: Ctx[R1, R2, W1, W2, T1, T2, E1, E2],
    f: W2 => W3
) extends Ctx[R1, R2, W1, W3, T1, T2, E1, E2]

final case class MapError[+R1, -R2, -W1, W2, -T1, +T2, -E1, E2, +E3](
    ctx: Ctx[R1, R2, W1, W2, T1, T2, E1, E2],
    f: E2 => E3
) extends Ctx[R1, R2, W1, W2, T1, T2, E1, E3]

final case class MapTag[+R1, -R2, -W1, +W2, -T1, T2, +T3, -E1, E2, +E3](
    ctx: Ctx[R1, R2, W1, W2, T1, T2, E1, E2],
    f: T2 => T3
) extends Ctx[R1, R2, W1, W2, T1, T3, E1, E3]

final case class Instrument[+R1, R2, -W1, W2, -T1, T2, T3, -E1, E2, +E3](
    ctx: Ctx[R1, R2, W1, W2, T1, T2, E1, E2],
    f: Instrumentation[R2, W2, T2, T3]
) extends Ctx[R1, R2, W1, W2, T1, T3, E1, E3]

trait MIO[-R, +W, +T, +E, +A]

final case class CIAO[R1, -R2, W1, +W2, T1, +T2, E1, +E2, A](
    io: IO[R1, W1, T1, E1, A],
    ctx: Ctx[R1, R2, W1, W2, T1, T2, E1, E2]
) extends MIO[R2, W2, T2, E2, A]
