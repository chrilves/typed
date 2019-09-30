package typed
package category

import scala.collection.immutable.TreeSet
import predicates._

/************************************************
  *       Functors
  */
/** Generic functors between two categories */
trait Functor[C1 <: Category, C2 <: Category] {
  val dom: C1
  val cod: C2

  type mapType[A]
  def mapObj[A: dom.obj]: cod.obj[mapType[A]]
  def mapArr[A: dom.obj, B: dom.obj](
      f: dom.arr[A, B]
  ): cod.arr[mapType[A], mapType[B]]
}

object Functor {
  type Aux[C1 <: Category, C2 <: Category, F[_]] = Functor[C1, C2] {
    type mapType[A] = F[A]
  }
}

/** Functors from a category to itself */
trait EndoFunctor[C <: Category] extends Functor[C, C] {
  val cat: C

  final val dom: cat.type = cat
  final val cod: cat.type = cat

  import cat._

  def mapObj[A: obj]: obj[mapType[A]]
  def mapArr[A: obj, B: obj](f: arr[A, B]): arr[mapType[A], mapType[B]]
}

object EndoFunctor {
  type Aux[C <: Category, F[_]] = EndoFunctor[C] { type mapType[A] = F[A] }
}

/** Functors as in ScalaZ/Cats */
trait RegularFunctor[F[_]] extends EndoFunctor[Scal.type] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]

  val cat: Scal.type = Scal

  final type mapType[A] = F[A]
  def mapObj[A: AlwaysUnit]: AlwaysUnit[F[A]] = AlwaysUnit.SingleValue
  def mapArr[A: AlwaysUnit, B: AlwaysUnit](f: A => B): F[A] => F[B] =
    map[A, B](f) _
}

/*****************************************
  *  Example of functors
  */
/** The classic {{{List}}} fonctor */
object FunctorList extends RegularFunctor[List] {
  def map[A, B](f: A => B)(fa: List[A]): List[B] = fa.map(f)
}

/** {{{TreeSet}}} also is a functor but between different categories.
  * Its domain is types whose have an {{{Ordering}}} and codomain is
  * types of the form {{{TreeSet[elem]}}}
  */
object FunctorTreeSet
    extends Functor[OrderedCategory.type, TreeSetCategory.type] {
  val dom: OrderedCategory.type = OrderedCategory
  val cod: TreeSetCategory.type = TreeSetCategory

  final type mapType[A] = TreeSet[A]

  def mapObj[A: Ordering]: IsTreeSet[TreeSet[A]] =
    IsTreeSet.Proof[A](implicitly[Ordering[A]])

  def mapArr[A: Ordering, B: Ordering](f: A => B): TreeSet[A] => TreeSet[B] =
    (setA: TreeSet[A]) => {
      val empty = TreeSet.empty[B](implicitly[Ordering[B]])
      setA.foldLeft[TreeSet[B]](empty) {
        case (tree, elem) => tree + f(elem)
      }
    }
}

/** {{{TreeSet}}} is also an endo functor [[OrderedCategory]] to itself */
object EndoFunctorTreeSet extends EndoFunctor[OrderedCategory.type] {
  val cat: OrderedCategory.type = OrderedCategory

  final type mapType[A] = TreeSet[A]

  def mapObj[A: Ordering]: Ordering[TreeSet[A]] =
    new Ordering[TreeSet[A]] {
      def compare(x: TreeSet[A], y: TreeSet[A]): Int = {
        def aux(l: TreeSet[A], r: TreeSet[A]): Int =
          (l.headOption, r.headOption) match {
            case (None, None) => 0
            case (None, _)    => -1
            case (_, None)    => 1
            case (Some(hdL), Some(hdR)) =>
              Ordering[A].compare(hdL, hdR)
          }
        aux(x -- y, y -- x)
      }
    }

  def mapArr[A: Ordering, B: Ordering](f: A => B): TreeSet[A] => TreeSet[B] =
    (setA: TreeSet[A]) => {
      val empty = TreeSet.empty[B](implicitly[Ordering[B]])
      setA.foldLeft[TreeSet[B]](empty) {
        case (tree, elem) => tree + f(elem)
      }
    }
}
