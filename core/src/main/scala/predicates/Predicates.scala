package typed
package predicates

import scala.collection.immutable.TreeSet

/** A proof that types {{{A}}} and {{{B}}}
  * are the same type.
  */
sealed trait EqT[A, B] {
  def fold[F[_, _]](v: EqT.Elim[F]): F[A, B]

  @inline final def to[G[_]]: G[A] =:= G[B] = {
    type F[X, Y] = G[X] =:= G[Y]
    fold[F] {
      new EqT.Elim[F] {
        def apply[Z]: F[Z, Z] = implicitly[G[Z] =:= G[Z]]
      }
    }
  }

  @inline final def from[G[_]]: G[B] =:= G[A] = {
    type F[X, Y] = G[Y] =:= G[X]
    fold[F] {
      new EqT.Elim[F] {
        def apply[Z]: F[Z, Z] = implicitly[G[Z] =:= G[Z]]
      }
    }
  }
}

object EqT {
  trait Elim[F[_, _]] {
    def apply[X]: F[X, X]
  }

  final case class Proof[A]() extends EqT[A, A] {
    def fold[F[_, _]](v: EqT.Elim[F]): F[A, A] = v[A]
  }

  @inline def proof[A]: EqT[A, A] = Proof[A]()
}

/** A proof that the type {{{A}}} is of
  * the form {{{F[elem]}}} for some type
  * {{{elem}}}.
  */
sealed abstract class IsA[F[_], A] {
  type elem
  def fold[G[_]](p: G[F[elem]]): G[A]

  @inline implicit final def to[H[_]]: H[A] =:= H[F[elem]] = {
    type G[X] = H[X] =:= H[F[elem]]
    fold[G](implicitly[H[F[elem]] =:= H[F[elem]]])
  }

  @inline implicit final def from[H[_]]: H[F[elem]] =:= H[A] = {
    type G[X] = H[F[elem]] =:= H[X]
    fold[G](implicitly[H[F[elem]] =:= H[F[elem]]])
  }

  def eqT: EqT[F[elem], A] = {
    type G[X] = EqT[F[elem], X]
    fold[G](EqT.proof[F[elem]])
  }
}
object IsA {
  final case class Proof[F[_], X]() extends IsA[F, F[X]] {
    final type elem = X
    @inline final def fold[G[_]](p: G[F[elem]]): G[F[X]] = p
  }

  @inline implicit def proof[F[_], elem]: IsA[F, F[elem]] =
    Proof[F, elem]()
}

/** A type-class proving that the type
  * {{{A}}} is of the form {{{TreeSet[elem]}}}.
  */
sealed abstract class IsTreeSet[A] {
  type elem
  val orderedElem: Ordering[elem]

  def fold[F[_]](elim: IsTreeSet.Elim[F]): F[A]
}
object IsTreeSet {
  final case class Proof[elem_](orderedElem: Ordering[elem_])
      extends IsTreeSet[TreeSet[elem_]] {
    final type elem = elem_

    @inline def fold[F[_]](f: IsTreeSet.Elim[F]): F[TreeSet[elem]] =
      f[elem](orderedElem)
  }

  /** Elimination rule for Proof */
  trait Elim[F[_]] {
    def apply[elem](value: Ordering[elem]): F[TreeSet[elem]]
  }
}
