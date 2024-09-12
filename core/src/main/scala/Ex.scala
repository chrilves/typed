package typed

sealed abstract class Ex[F[_]] {
  type hidden
  val value: hidden
  val evidence: F[hidden]

  def fold[T](f: Ex.Elim[F, T]): T
}

object Ex:
  trait Elim[F[_], T]:
    def apply[hidden](value: hidden)(using evidence: F[hidden]): T

  inline def apply[F[_]]: Builder[F] = new Builder[F]

  final class Builder[F[_]]:
    def apply[A](value_ : A)(using evidence_ : F[A]): Ex[F] =
      new Ex[F]:
        type hidden = A
        val value: hidden       = value_
        val evidence: F[hidden] = evidence_

        def fold[T](f: Elim[F, T]): T = f[hidden](value_)(using evidence_)
