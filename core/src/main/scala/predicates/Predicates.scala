package typed
package predicates

/** A proof that types {{{A}}} and {{{B}}} are the same type.
  */
enum EqT[A, B]:
  case Proof[A]() extends EqT[A, A]

  def fold[F[_, _]](v: EqT.Elim[F]): F[A, B] =
    this match
      case Proof() => v[A]

  final def to[G[_]]: G[A] =:= G[B] =
    type F[X, Y] = G[X] =:= G[Y]
    fold[F] {
      new EqT.Elim[F]:
        def apply[Z]: F[Z, Z] = summon[G[Z] =:= G[Z]]
    }

  final def from[G[_]]: G[B] =:= G[A] =
    type F[X, Y] = G[Y] =:= G[X]
    fold[F] {
      new EqT.Elim[F]:
        def apply[Z]: F[Z, Z] = summon[G[Z] =:= G[Z]]
    }

object EqT:
  trait Elim[F[_, _]]:
    def apply[X]: F[X, X]

  inline def proof[A]: EqT[A, A] = Proof[A]()

/** A proof that the type {{{A}}} is of the form {{{F[elem]}}} for some type {{{elem}}}.
  */
sealed abstract class IsA[F[_], A]:
  type elem
  def fold[G[_]](p: G[F[elem]]): G[A]

  inline given to[H[_]]: (H[A] =:= H[F[elem]]) =
    type G[X] = H[X] =:= H[F[elem]]
    fold[G](summon[H[F[elem]] =:= H[F[elem]]])

  inline given from[H[_]]: (H[F[elem]] =:= H[A]) =
    type G[X] = H[F[elem]] =:= H[X]
    fold[G](summon[H[F[elem]] =:= H[F[elem]]])

  def eqT: EqT[F[elem], A] =
    type G[X] = EqT[F[elem], X]
    fold[G](EqT.proof[F[elem]])

object IsA:
  final case class Proof[F[_], X]() extends IsA[F, F[X]]:
    final type elem = X
    inline final def fold[G[_]](p: G[F[elem]]): G[F[X]] = p

  inline given proof[F[_], elem]: IsA[F, F[elem]] =
    Proof[F, elem]()
