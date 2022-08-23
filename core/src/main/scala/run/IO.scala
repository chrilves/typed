package typed
package run

import scala.annotation.tailrec

trait Semigroup[A]:
  def combine(a1: A, a2: A): A

object Semigroup:
  inline def apply[A](using ev: Semigroup[A]): ev.type =
    ev

trait Monoid[A] extends Semigroup[A]:
  def zero: A

object Monoid:
  inline def apply[A](using ev: Monoid[A]): ev.type = ev

enum IO[-R, +W, +T, +E, +A]:
  /** Reader operation: get the input value of type R */
  private case Read[R]() extends IO[R, Nothing, Nothing, Nothing, R]

  private case MapRead[R1, -R2, +W, +T, +E, +A](
      io: IO[R1, W, T, E, A],
      fun: R2 => R1
  ) extends IO[R2, W, T, E, A]

  /** Writer operation: output some value of type W */
  private case Write[+W](value: W) extends IO[Any, W, Nothing, Nothing, Unit]

  private case MapWrite[-R, W1, +W2, +T, +E, +A](
      io: IO[R, W1, T, E, A],
      fun: W1 => W2
  ) extends IO[R, W2, T, E, A]

  /** Tag operation: tag some computation with tag of type T */
  private case Tag[-R, +W, +T, +E, +A](
      tag: T,
      io: IO[R, W, T, E, A]
  ) extends IO[R, W, T, E, A]

  private case MapTag[-R, +W, T1, +T2, +E, +A](
      io: IO[R, W, T1, E, A],
      fun: T1 => T2
  ) extends IO[R, W, T2, E, A]

  /** Monad Error operation: fail with error of type E */
  private case Error[+E](error: E) extends IO[Any, Nothing, Nothing, E, Nothing]

  /** Monad Error operation: catch errors of type E */
  private case CatchError[-R, +W, +T, E1, +E2, +A](
      io: IO[R, W, T, E1, A],
      handler: E1 => IO[R, W, T, E2, A]
  )(using val errorSemiGroupInstance: Semigroup[E1]) extends IO[R, W, T, E2, A]

  private case MapError[-R, +W, +T, E1, +E2, +A](
      io: IO[R, W, T, E1, A],
      fun: E1 => E2
  ) extends IO[R, W, T, E2, A]

  private case Pure[+A](value: A) extends IO[Any, Nothing, Nothing, Nothing, A]

  private case Ap[-R, +W, +T, +E, X, +A](
      fun: IO[R, W, T, E, X => A],
      arg: IO[R, W, T, E, X]
  ) extends IO[R, W, T, E, A]

  private case Flatten[-R, +W, +T, +E, +A](
      value: IO[R, W, T, E, IO[R, W, T, E, A]]
  ) extends IO[R, W, T, E, A]

  private case Defer[-R, +W, +T, +E, +A](
      deferred: () => IO[R, W, T, E, A]
  ) extends IO[R, W, T, E, A]

  inline final def mapRead[R2](f: R2 => R): IO[R2, W, T, E, A] =
    IO.mapRead(this)(f)

  inline final def mapWrite[W2](f: W => W2): IO[R, W2, T, E, A] =
    IO.mapWrite(this)(f)

  inline final def mapTag[T2](f: T => T2): IO[R, W, T2, E, A] =
    IO.mapTag(this)(f)

  inline final def mapError[E2](f: E => E2): IO[R, W, T, E2, A] =
    IO.mapError[R, W, T, E, E2, A](this)(f)

  inline final def map[A2](f: A => A2): IO[R, W, T, E, A2] =
    IO.map(this)(f)

  inline final def tag[T2 >: T](t: T2): IO[R, W, T2, E, A] =
    IO.tag(t)(this)

  inline final def flatMap[R2 <: R, W2 >: W, T2 >: T, E2 >: E, B](
      f: A => IO[R2, W2, T2, E2, B]
  ): IO[R2, W2, T2, E2, B] =
    IO.flatMap[R2, W2, T2, E2, A, B](this)(f)

  inline final def ap[R2 <: R, W2 >: W, T2 >: T, E2 >: E, B](
      f: IO[R2, W2, T2, E2, A => B]
  ): IO[R2, W2, T2, E2, B] =
    IO.ap(f, this)

  inline final def toProcess[E2 >: E](using
      E2: Semigroup[E2]
  ): Process[R, W, E2, A] =
    IO.toProcess[R, W, T, E2, A](this)

  inline final def run[W2 >: W, E2 >: E](
      r: R
  )(using
      W2: Semigroup[W2],
      E2: Semigroup[E2]
  ): (Option[W2], Either[E2, A]) =
    IO.run[R, W2, T, E2, A](this)(r)

object IO:
  inline def read[R]: IO[R, Nothing, Nothing, Nothing, R] =
    Read[R]()

  inline def mapRead[R1, R2, W, T, E, A](
      io: IO[R1, W, T, E, A]
  )(f: R2 => R1): IO[R2, W, T, E, A] =
    io match
      case _: Read[r] => map(Read[R2]())(f)
      case Pure(a)    => Pure(a)
      case Error(e)   => Error(e)
      case Write(w)   => Write(w)
      case _          => MapRead(io, f)

  inline def write[W](value: W): IO[Any, W, Nothing, Nothing, Unit] =
    Write[W](value)

  inline def mapWrite[R, W1, W2, T, E, A](
      io: IO[R, W1, T, E, A]
  )(f: W1 => W2): IO[R, W2, T, E, A] =
    io match
      case _: Read[r] => Read()
      case Pure(a)    => Pure(a)
      case Error(e)   => Error(e)
      case Write(w)   => Write(f(w))
      case _          => MapWrite(io, f)

  inline def tag[R, W, T, E, A](
      tag: T
  )(io: IO[R, W, T, E, A]): IO[R, W, T, E, A] =
    Tag(tag, io)

  inline def mapTag[R, W, T1, T2, E, A](
      io: IO[R, W, T1, E, A]
  )(f: T1 => T2): IO[R, W, T2, E, A] =
    def oneStep(m: IO[R, W, T1, E, A]): IO[R, W, T2, E, A] =
      m match
        case _: Read[r] => Read()
        case Pure(a)    => Pure(a)
        case Error(e)   => Error(e)
        case Write(w)   => Write(w)
        case _          => MapTag(m, f)

    io match
      case Tag(t, i) => Tag(f(t), oneStep(i))
      case _         => oneStep(io)

  inline def error[E](err: E): IO[Any, Nothing, Nothing, E, Nothing] =
    Error(err)

  inline def catchError[R, W, T, E1: Semigroup, E2, A](
      io: IO[R, W, T, E1, A]
  )(handler: E1 => IO[R, W, T, E2, A]): IO[R, W, T, E2, A] =
    io match
      case Pure(a)  => Pure(a)
      case Error(e) => handler(e)
      case _        => CatchError(io, handler)

  inline def mapError[R, W, T, E1, E2, A](
      io: IO[R, W, T, E1, A]
  )(f: E1 => E2): IO[R, W, T, E2, A] =
    io match
      case _: Read[r] => Read()
      case Pure(a)    => Pure(a)
      case Error(e)   => Error(f(e))
      case Write(w)   => Write(w)
      case _          => MapError(io, f)

  inline def pure[A](value: A): IO[Any, Nothing, Nothing, Nothing, A] =
    Pure(value)

  inline def ap[R, W, T, E, X, A](
      fun: IO[R, W, T, E, X => A],
      arg: IO[R, W, T, E, X]
  ): IO[R, W, T, E, A] =
    (fun, arg) match
      case (Error(f), Pure(_)) => Error(f)
      case (Pure(_), Error(a)) => Error(a)
      case (Pure(f), Pure(x))  => Pure(f(x))
      case (_, _)              => Ap(fun, arg)

  inline def flatten[R, W, T, E, A](
      value: IO[R, W, T, E, IO[R, W, T, E, A]]
  ): IO[R, W, T, E, A] =
    value match
      case Error(e) => Error(e)
      case Pure(io) => io
      case _        => Flatten(value)

  inline def defer[R, W, T, E, A](
      deferred: () => IO[R, W, T, E, A]
  ): IO[R, W, T, E, A] =
    Defer(deferred)

  inline def lazyli[R, W, T, E, A](
      deferred: => IO[R, W, T, E, A]
  ): IO[R, W, T, E, A] =
    Defer(() => deferred)

  inline def map[R, W, T, E, X, A](
      io: IO[R, W, T, E, X]
  )(f: X => A): IO[R, W, T, E, A] =
    ap(Pure(f), io)

  inline def flatMap[R, W, T, E, X, A](
      io: IO[R, W, T, E, X]
  )(f: X => IO[R, W, T, E, A]): IO[R, W, T, E, A] =
    flatten(map(io)(f))

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def toProcess[R, W, T, E: Semigroup, A](
      mainIO: IO[R, W, T, E, A]
  ): Process[R, W, E, A] =
    type Ret = Process[R, W, E, A]

    def auxTrick[R0, W0, T0, E0, E1: Semigroup, A0](
        io: IO[R0, W0, T0, E0, A0],
        fr: R => R0,
        fw: W0 => W,
        ft: T0 => T,
        fe: E0 => E1,
        k: A0 => Ret,
        h: E1 => Ret
    ): Ret =
      aux(io, fr, fw, ft, fe, k, h)

    @tailrec def aux[R0, W0, T0, E0, E1: Semigroup, A0](
        io: IO[R0, W0, T0, E0, A0],
        fr: R => R0,
        fw: W0 => W,
        ft: T0 => T,
        fe: E0 => E1,
        k: A0 => Ret,
        h: E1 => Ret
    ): Ret =
      io match
        case Pure(a)        => k(a)
        case Error(e1)      => h(fe(e1))
        case Read()         => Process.Input(fr.andThen(k))
        case Write(w)       => Process.Output(fw(w), () => k(()))
        case Tag(_, i)      => aux(i, fr, fw, ft, fe, k, h)
        case Defer(d)       => aux(d(), fr, fw, ft, fe, k, h)
        case MapRead(i, f)  => aux(i, fr.andThen(f), fw, ft, fe, k, h)
        case MapWrite(i, f) => aux(i, fr, f.andThen(fw), ft, fe, k, h)
        case MapTag(i, f)   => aux(i, fr, fw, f.andThen(ft), fe, k, h)
        case MapError(i, f) => aux(i, fr, fw, ft, f.andThen(fe), k, h)
        case ap: Ap[R0, W0, T0, E0, x, A0] =>
          aux(
            ap.fun,
            fr,
            fw,
            ft,
            fe,
            (f: x => A0) => auxTrick(ap.arg, fr, fw, ft, fe, (x: x) => k(f(x)), h),
            (e1: E1) =>
              auxTrick(
                ap.arg,
                fr,
                fw,
                ft,
                fe,
                (_: x) => h(e1),
                (e2: E1) => h(Semigroup[E1].combine(e1, e2))
              )
          )
        case Flatten(v) =>
          aux[R0, W0, T0, E0, E1, IO[R0, W0, T0, E0, A0]](
            v,
            fr,
            fw,
            ft,
            fe,
            (i: IO[R0, W0, T0, E0, A0]) => auxTrick(i, fr, fw, ft, fe, k, h),
            h
          )
        case ce: CatchError[R0, W0, T0, e, E0, A0] =>
          aux[R0, W0, T0, e, e, A0](
            ce.io,
            fr,
            fw,
            ft,
            identity[e],
            k,
            ce.handler.andThen(auxTrick(_, fr, fw, ft, fe, k, h))
          )(ce.errorSemiGroupInstance)

    aux[R, W, T, E, E, A](
      mainIO,
      identity[R],
      identity[W],
      identity[T],
      identity[E],
      Process.Result(_),
      Process.Error(_)
    )

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def run[R, W: Semigroup, T, E: Semigroup, A](
      mainIO: IO[R, W, T, E, A]
  )(r: R): (Option[W], Either[E, A]) =

    def mergeWrite(ow1: Option[W], ow2: Option[W]): Option[W] =
      (ow1, ow2) match {
        case (None, _) => ow2
        case (_, None) => ow1
        case (Some(w1), Some(w2)) =>
          Some(Semigroup[W].combine(w1, w2))
      }

    def aux[R0, W0, T0, E0, E1: Semigroup, A0](
        io: IO[R0, W0, T0, E0, A0],
        fr: R => R0,
        fw: W0 => W,
        ft: T0 => T,
        fe: E0 => E1
    ): (Option[W], Either[E1, A0]) =
      io match
        case Pure(a)        => (None, Right(a))
        case Error(e1)      => (None, Left(fe(e1)))
        case Read()         => (None, Right(fr(r)))
        case Write(w)       => (Some(fw(w)), Right(()))
        case Tag(_, i)      => aux(i, fr, fw, ft, fe)
        case Defer(d)       => aux(d(), fr, fw, ft, fe)
        case MapRead(i, f)  => aux(i, fr.andThen(f), fw, ft, fe)
        case MapWrite(i, f) => aux(i, fr, f.andThen(fw), ft, fe)
        case MapTag(i, f)   => aux(i, fr, fw, f.andThen(ft), fe)
        case MapError(i, f) => aux(i, fr, fw, ft, f.andThen(fe))
        case ap: Ap[R0, W0, T0, E0, x, A0] =>
          val (ow1, rf) = aux(ap.fun, fr, fw, ft, fe)
          val (ow2, ra) = aux(ap.arg, fr, fw, ft, fe)

          val ret: Either[E1, A0] =
            (rf, ra) match
              case (Right(f), Right(a)) => Right(f(a))
              case (Left(x), Left(y))   => Left(Semigroup[E1].combine(x, y))
              case (Left(x), _)         => Left(x)
              case (_, Left(y))         => Left(y)

          (mergeWrite(ow1, ow2), ret)

        case Flatten(v) =>
          val (ow1, ret1) = aux(v, fr, fw, ft, fe)

          ret1 match
            case Left(e) => (ow1, Left(e))
            case Right(io2) =>
              val (ow2, ret2) = aux(io2, fr, fw, ft, fe)
              (mergeWrite(ow1, ow2), ret2)

        case ce: CatchError[R0, W0, T0, e, E0, A0] =>
          val (ow1, ret1) =
            aux(ce.io, fr, fw, ft, identity[e])(ce.errorSemiGroupInstance)
          ret1 match {
            case Left(e1) =>
              val (ow2, ret2) = aux(ce.handler(e1), fr, fw, ft, fe)
              (mergeWrite(ow1, ow2), ret2)
            case Right(a) => (ow1, Right(a))
          }

    aux[R, W, T, E, E, A](
      mainIO,
      identity[R],
      identity[W],
      identity[T],
      identity[E]
    )
