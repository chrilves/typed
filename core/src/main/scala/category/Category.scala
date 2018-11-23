package typed
package category

import scala.language.higherKinds
import predicates._

/*****************************************
  *  A Category
  */
trait Category { cat =>
  type obj[A]
  type arr[A, B]

  def id[A: obj]: arr[A, A]
  def andThen[A: obj, B: obj, C: obj](f: arr[A, B], g: arr[B, C]): arr[A, C]

  trait Laws extends EqArrow {
    def idNeutralLeft[A: obj, B: obj](f: arr[A, B]): Boolean =
      id[A].andThen(f) === f

    def idNeutralRight[A: obj, B: obj](f: arr[A, B]): Boolean =
      f.andThen(id[B]) === f

    def associtivity[A: obj, B: obj, C: obj, D: obj](
        f: arr[A, B],
        g: arr[B, C],
        h: arr[C, D]
    ): Boolean =
      f.andThen(g).andThen(h) === f.andThen(g.andThen(h))
  }

  trait EqArrow {
    def eqArrow[A: obj, B: obj](f: arr[A, B], g: arr[A, B]): Boolean

    implicit final class ArrowEqOps[A, B](val self: arr[A, B]) {
      @inline final def ===(g: arr[A, B])(implicit A: obj[A],
                                          B: obj[B]): Boolean =
        eqArrow(self, g)
    }
  }

  implicit final class ArrowOps[A, B](val self: arr[A, B]) {
    @inline final def andThen[C: obj](g: arr[B, C])(implicit A: obj[A],
                                                    B: obj[B]): arr[A, C] =
      cat.andThen(self, g)
  }
}

object Category {
  type Aux[objects[_], arrow[_, _]] = Category {
    type obj[A] = objects[A]
    type arr[A, B] = arrow[A, B]
  }

  def constrainedScal[TypeClass[_]]: ConstrainedScal[TypeClass] =
    new ConstrainedScal[TypeClass] {}
}

/*****************************************
  *  Example of Categories
  */
/** The category whose objects are types {{{A}}}
  * for which there is an instance of the
  * type class {{{TypeClass[A]}}} and arrows
  * are functions between these types.
  */
trait ConstrainedScal[TypeClass[_]] extends Category {
  final type obj[A] = TypeClass[A]
  final type arr[A, B] = A => B

  @inline final def id[A: TypeClass]: A => A = identity[A]
  @inline final def andThen[A: TypeClass, B: TypeClass, C: TypeClass](
      f: A => B,
      g: B => C): A => C =
    f.andThen(g)
}

/** Type class for which every
  * type has an instance.
  * Equivalent to no constraints
  * on the type {{{A}}}
  */
sealed abstract class AlwaysUnit[+A]
object AlwaysUnit {
  implicit final case object SingleValue extends AlwaysUnit[Nothing]
}

/** Category whose objects are types and
  * arrow functions
  */
object Scal extends ConstrainedScal[AlwaysUnit]

/** Category whose objects are types that can
  * be ordered (they have an instance of
  * {{{Ordering[A]}}}
  */
object OrderedCategory extends ConstrainedScal[Ordering]

/** Category whose objects are types of the form
  * {{{TreeSet[elem]}}}.
  */
object TreeSetCategory extends ConstrainedScal[IsTreeSet]
