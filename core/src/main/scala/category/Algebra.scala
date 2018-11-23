package typed
package category

trait Algebra[C <: Category, F <: EndoFunctor[C], Obj] {
  val cat: C
  val functor: F

  val objOfCat: cat.obj[Obj]
  val algebra: cat.arr[functor.mapType[Obj], Obj]
}

trait InitialAlgebra[C <: Category, F <: EndoFunctor[C], I]
    extends Algebra[C, F, I] {
  def fold[A: cat.obj](f: cat.arr[functor.mapType[A], A]): cat.arr[I, A]
}

trait CoAlgebra[C <: Category, F <: EndoFunctor[C], Obj] {
  val cat: C
  val functor: F

  val objOfCat: cat.obj[Obj]
  val coAlgebra: cat.arr[Obj, functor.mapType[Obj]]
}

trait TerminalCoAlgebra[C <: Category, F <: EndoFunctor[C], T]
    extends Algebra[C, F, T] {
  def unfold[A: cat.obj](f: cat.arr[functor.mapType[A], A]): cat.arr[A, T]
}
