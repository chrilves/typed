package typed
package category

import scala.collection.immutable.TreeSet
import predicates._
import typed.category.Category.AllTypes

/** ********* Functors *******************
  */
/** Generic functors between two categories */
trait Functor[O1[_], ~>[_, _], O2[_], ~~>[_, _], F[_]]:
  def domain: Category[O1, ~>]
  def codomain: Category[O2, ~>]

  def validity[A: O1]: O2[F[A]]
  def map[A: O1, B: O1](f: A ~> B): F[A] ~~> F[B]

trait FunctorOfFunctions[O1[_], O2[_], F[_]] extends Functor[O1, Function, O2, Function, F]:
  def domain: Category[O1, Function]   = new CategoryOfFunctions[O1]
  def codomain: Category[O2, Function] = new CategoryOfFunctions[O2]

trait EndoFunctor[O[_], ~>[_, _], F[_]] extends Functor[O, ~>, O, ~>, F]:
  def category: Category[O, ~>]
  def domain: Category[O, ~>]   = category
  def codomain: Category[O, ~>] = category

trait EndoFunctorOfFunctions[O[_], F[_]]
    extends EndoFunctor[O, Function, F]
    with FunctorOfFunctions[O, O, F]:
  final val category: Category[O, Function]    = new CategoryOfFunctions[O]
  override def domain: Category[O, Function]   = super.domain
  override def codomain: Category[O, Function] = super.codomain

trait UsualFunctor[F[_]] extends EndoFunctorOfFunctions[AllTypes, F]:
  def validity[A: AllTypes]: AllTypes[F[A]] = summon

/** Example of functors
  */
/** The classic {{{List}}} fonctor */
object FunctorList extends UsualFunctor[List]:
  def map[A: AllTypes, B: AllTypes](f: A => B): List[A] => List[B] =
    (l: List[A]) => l.map(f)

/** {{{TreeSet}}} also is a functor but between different categories. Its domain is types whose have
  * an {{{Ordering}}} and codomain is types of the form {{{TreeSet[elem]}}}
  */
object FunctorTreeSet extends FunctorOfFunctions[Ordering, IsA[TreeSet, *], TreeSet]:

  def validity[A: Ordering]: IsA[TreeSet, TreeSet[A]] =
    IsA.proof[TreeSet, A]

  def map[A: Ordering, B: Ordering](f: A => B): TreeSet[A] => TreeSet[B] =
    (setA: TreeSet[A]) => {
      val empty = TreeSet.empty[B](summon[Ordering[B]])
      setA.foldLeft[TreeSet[B]](empty) { case (tree, elem) =>
        tree + f(elem)
      }
    }

/** {{{TreeSet}}} is also an endo functor [[OrderedCategory]] to itself */
object EndoFunctorTreeSet extends EndoFunctorOfFunctions[Ordering, TreeSet]:
  def validity[A: Ordering]: Ordering[TreeSet[A]] =
    new Ordering[TreeSet[A]]:
      def compare(x: TreeSet[A], y: TreeSet[A]): Int =
        def aux(l: TreeSet[A], r: TreeSet[A]): Int =
          (l.headOption, r.headOption) match {
            case (None, None) => 0
            case (None, _)    => -1
            case (_, None)    => 1
            case (Some(hdL), Some(hdR)) =>
              Ordering[A].compare(hdL, hdR)
          }
        aux(x -- y, y -- x)

  def map[A: Ordering, B: Ordering](f: A => B): TreeSet[A] => TreeSet[B] =
    (setA: TreeSet[A]) => {
      val empty = TreeSet.empty[B](summon[Ordering[B]])
      setA.foldLeft[TreeSet[B]](empty) { case (tree, elem) =>
        tree + f(elem)
      }
    }
