package typed
package category

trait Monad[O[_], ~>[_, _], F[_]] extends EndoFunctor[O, ~>, F]:
  def pure[A: O]: A ~> F[A]
  def flatMap[A: O, B: O](f: A ~> F[B]): F[A] ~> F[B]

  final def map[A: O, B: O](f: A ~> B): F[A] ~> F[B] =
    given ofb: O[F[B]] = validity[B]
    flatMap(category.andThen(f, pure[B]))
