package typed
package predicates

final case class &&[F[_], G[_], A](first: F[A], second: G[A])

sealed abstract class ConjK[A] {
  def fold[R](unitK: R)(consK: ConjK.Elim[A, R]): R
}
object ConjK {
  final case class UnitK[A]() extends ConjK[A] {
    def fold[R](unitK: R)(consK: Elim[A, R]): R = unitK
  }
  final case class ConsK[A, F[_], B <: ConjK[A]](val head: F[A], tail: B)
      extends ConjK[A] {
    def fold[R](unitK: R)(consK: Elim[A, R]): R = {
      val rec: R = tail.fold[R](unitK)(consK)
      consK[F, R](head, rec)
    }
  }

  trait Elim[A, R] {
    def apply[F[_], B <: R](head: F[A], tail: B): R
  }
}
