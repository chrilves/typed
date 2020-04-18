package typed
package category

/*****************************************
  *  A Category
  */
trait Category[O[_], ~>[_, _]] { cat =>
  def id[A: O]: A ~> A
  def andThen[A: O, B: O, C: O](f: A ~> B, g: B ~> C): A ~> C

  trait Laws {
    def eqArrow[A: O, B: O](f: A ~> B, g: A ~> B): Boolean

    final def idNeutralLeft[A: O, B: O](f: A ~> B): Boolean =
      eqArrow(andThen(id[A], f), f)

    final def idNeutralRight[A: O, B: O](f: A ~> B): Boolean =
      eqArrow(andThen(f, id[B]), f)

    final def associativity[A: O, B: O, C: O, D: O](
        f: A ~> B,
        g: B ~> C,
        h: C ~> D
    ): Boolean =
      eqArrow(andThen(f, andThen(g, h)), andThen(andThen(f, g), h))
  }
}

object Category {
  type AllTypes[A] = A <:< Any
}

class CategoryOfFunctions[O[_]] extends Category[O, Function] {
  final def id[A: O]: A => A = identity[A]
  final def andThen[A: O, B: O, C: O](f: A => B, g: B => C): A => C =
    f.andThen(g)
}

/** Category whose objects are types and
  * arrow functions
  */
object Scal extends CategoryOfFunctions[Category.AllTypes]

/** Category whose objects are types that can
  * be ordered (they have an instance of {{{Ordering[A]}}}
  */
object OrderedCategory extends CategoryOfFunctions[Ordering]
