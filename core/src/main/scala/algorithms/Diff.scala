package typed
package algorithms

/** Represents the differentiation list of two sequences.
  * i.e. what to insert, delete, replace or keep to transform
  * the first list into the second one.
  *
  * See [[Diff.apply]] et [[Diff.myers]] for more.
  */
sealed abstract class Diff[+A, +B]

object Diff {

  /** Format the differences */
  def showDiffs[A, B](l: List[Diff[A, B]]): String =
    l.map {
        case Insertion(b)      => s"+$b"
        case Deletion(a)       => s"-$a"
        case Identical(a, b)   => s"$a=$b"
        case Replacement(a, b) => s"$a->$b"
      }
      .mkString(",")

  final case class Insertion[+B](value: B) extends Diff[Nothing, B]
  final case class Deletion[+A](value: A) extends Diff[A, Nothing]
  final case class Replacement[+A, +B](from: A, to: B) extends Diff[A, B]

  /** Represent identical elements.
    * They can still be different but only considered identical.
    */
  final case class Identical[+A, +B](from: A, to: B) extends Diff[A, B]

  /** Compute the list of differences between sequences {{a1}} and {{a2}}
    * by comparing elements via the function {{eq}}
    *
    * Example:
    *  {{Diff((x:Char, y:Char) => x === y)("TABLE".toList, "PARBOILED".toList)}}
    *
    * to compute what to insert into, delete from, replace into or keep from "TABLE"
    * to get "PARBOILED"
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Var",
      "org.wartremover.warts.TraversableOps",
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  def apply[A, B](
      eq: (A, B) => Boolean
  )(a1: List[A], a2: List[B]): (Int, List[Diff[A, B]]) = {
    type Q = (Int, List[Diff[A, B]])
    type T = Array[Q]

    val n2 = a2.size
    val d = a2.zipWithIndex

    @scala.annotation.tailrec
    def auxI(l0: T, l1: T, lg: Seq[(A, Int)]): Q =
      lg match {
        case Nil =>
          val (c, l) = l0(n2)
          (c, l.reverse)

        case (c1, i) :: tl =>
          l1(0) = (i + 1, Deletion(c1) :: l0(0)._2)

          d.foreach {
            case (c2, j) =>
              def choix(p: Q, increment: Int, modif: Diff[A, B]): Q =
                (p._1 + increment, modif :: p._2)

              val l = List(
                choix(l0(j + 1), 1, Deletion(c1)),
                choix(l1(j), 1, Insertion(c2)), {
                  val p = l0(j)
                  val (inc, mod): (Int, Diff[A, B]) =
                    if (eq(c1, c2)) (0, Identical(c1, c2))
                    else (1, Replacement(c1, c2))
                  choix(p, inc, mod)
                }
              )
              l1(j + 1) = l.sortBy(_._1).head
          }

          auxI(l1, l0, tl)
      }

    val _l0: T = Array.fill(n2 + 1)((0, Nil))
    val _l1: T = Array.fill(n2 + 1)((0, Nil))

    def init_l0(j: Int)(l: List[Diff[A, B]]): List[Diff[A, B]] = {
      _l0(j) = (j, l)
      l
    }

    d.foldLeft { init_l0(0)(Nil) } {
      case (s, (c2, j)) => init_l0(j + 1)(Insertion(c2) :: s)
    }

    auxI(_l0, _l1, a1.zipWithIndex)
  }

  /** Compute the list of differences between sequences {{a1}} and {{a2}}
    * by comparing elements via the function {{eq}}
    *
    * Example:
    *  {{Diff((x:Char, y:Char) => x === y)("TABLE".toList, "PARBOILED".toList)}}
    *
    * to compute what to insert into, delete from, replace into or keep from "TABLE"
    * to get "PARBOILED"
    */
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def myers[A, B](
      eq: (A, B) => Boolean
  )(l: Array[A], r: Array[B]): (Int, List[Diff[A, B]]) = {
    import scala.annotation._

    type D = List[Diff[A, B]]

    (l.size, r.size) match {
      case (0, nr)  => (nr, r.toList.map(Insertion(_)))
      case (nl, 0)  => (nl, l.toList.map(Deletion(_)))
      case (nl, nr) =>
        /* Creation of the array V (with indices from -vmax to +vmax, both included)

            INVARIANT:

            v[k] = (x,p)

            means that:

            applying p on l[0..x]  == r[0..x-k]

            the cursor on l is positioned at x
            the cursor on r is positioned at x-k

            p is the diff between l[0..x] and r[0..x-k]
         */
        object v {
          /* The maximal number of differences between l and r
             Reached when we need to Delete all one (l or r)
             and insert all the other (l or r)
           */
          val vmax: Int = nl + nr

          /* V considered uninitialized!
             No read to v[k] until v[k] is set once
           */
          val _v: Array[(Int, D)] = Array.fill(2 * vmax + 1)((0, Nil: D))

          @inline def offset(i: Int): Int = i + vmax

          @inline def apply(k: Int): (Int, D) =
            _v(offset(k))
          @inline def update(k: Int, a: (Int, D)): Unit =
            _v(offset(k)) = a
        }

        // Initialisation: to safely read v[1]
        v(1) = (0, Nil: D)

        /* Computes the number of difference and the diff list
         *
         * @param d the number of difference we are considering now
         * @return (n, p) where n is the number of difference and p the diff list
         */
        @tailrec
        def auxD(d: Int): (Int, D) = {
          /* d = number of difference we are testing.
             If there are actually d differneces the algorithm
             terminates without calling auxD(d+1)
           */

          /* Try to advances the cursors l[0..x] and r[0..y] at most as possible
           * Considers that there are exactly d differences.
           * Using the previously computed array `v` for d = 0..(d-1)
           * to test from: l[0..x] vs r[0..(x-d)]
           *           to: l[0..x] vs r[0..(x+d)]
           *
           * @param k k=x-y
           * @return
           */
          @tailrec
          def auxK(k: Int): Option[(Int, D)] = {
            if (k > d)
              /* If there is `d` differences, then x-d <= y <= x+d
                 Where applying p on l[0..x] == r[0..y]

                 So k = x - y <= d
                 No need to test bigger k for this d
               */
              None
            else {
              @inline
              def y(x: Int): Int = x - k

              /* Try to advance the cursors at most as possible
               * Try to find the biggest n for which: l[x..x+n] == r[y..y+n]
               *
               * @param x
               * @param p
               * @return the new position x and the new diff list p
               */
              @tailrec
              @inline
              def furthest(x: Int, p: D): (Int, D) = {
                if (x < nl && y(x) < nr && eq(l(x), r(y(x))))
                  furthest(x + 1, Identical(l(x), r(y(x))) :: p)
                else
                  (x, p)
              }

              val (x, p, suppr) =
                if (k == -d || ((k != d) && v(k - 1)._1 < v(k + 1)._1))
                  v(k + 1) match {
                    case (_x, _p) => (_x, _p, false)
                  } else
                  v(k - 1) match {
                    case (_x, _p) => (_x + 1, _p, true)
                  }

              if (x > nl || y(x) > nr || y(x) < 0) auxK(k + 2)
              else {
                val p2: D =
                  if (suppr)
                    if (x > 0) Deletion(l((x - 1))) :: p else p
                  else if (y(x) > 0) Insertion(r((y(x) - 1))) :: p
                  else p

                val fxp = furthest(x, p2)

                if (fxp._1 >= nl && y(fxp._1) >= nr)
                  Some((d, fxp._2.reverse))
                else {
                  v(k) = fxp
                  auxK(k + 2)
                }
              }
            }
          }

          auxK(-d) match {
            case Some(ret) => ret
            case _         => auxD(d + 1)
          }
        }
        auxD(0)
    }
  }

  /** Specialization of [[Diff.myers]] to Strings */
  def myersChaines(s1: String, s2: String): (Int, List[Diff[Char, Char]]) =
    myers((x: Char, y: Char) => x === y)(s1.toArray, s2.toArray)
}
