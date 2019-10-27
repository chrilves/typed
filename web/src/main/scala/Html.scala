package typed
package web

import org.scalajs.dom.raw._
import org.scalajs.dom._
import scalajs.js

/** An HTML Attribute */
object Attribute {

  /** The namespace of the HTML attribute. It is wise to always set the namespace. */
  final case class Namespace(value: String) extends AnyVal

  /** An HTML attribute is actually a pair of a name and a namespace */
  final case class Key(value: String, namespace: Option[Namespace])

  object Key {
    val `class`: Key = Key("class", None)
    val style: Key = Key("style", None)
    val mergeable: Set[Key] = Set(`class`, style)
  }

  /** The value of the HTML attribute */
  final case class Value(value: String) extends AnyVal
}

/** To be given to addEventListener */
final case class Reaction[+A](
    `type`: String,
    reaction: js.Function1[_ <: Event, A]
) {
  def map[B](f: A => B): Reaction[B] =
    Reaction(`type`, reaction.andThen(f))
}

object Reaction {
  @inline
  def on[T <: Event, A](
      `type`: String
  )(f: js.Function1[T, A]): Reaction[A] =
    Reaction(`type`, (e: T) => {
      e.stopPropagation()
      f(e)
    })

  @inline final def keyup[A](handler: KeyboardEvent => A): Reaction[A] =
    on[KeyboardEvent, A]("keyup")(handler)
  @inline final def keydown[A](handler: KeyboardEvent => A): Reaction[A] =
    on[KeyboardEvent, A]("keydown")(handler)
  @inline final def keypress[A](handler: KeyboardEvent => A): Reaction[A] =
    on[KeyboardEvent, A]("keypress")(handler)

  @inline final def mouseup[A](handler: MouseEvent => A): Reaction[A] =
    on[MouseEvent, A]("mouseup")(handler)
  @inline final def mousedown[A](handler: MouseEvent => A): Reaction[A] =
    on[MouseEvent, A]("mousedown")(handler)
  @inline final def dblclick[A](handler: MouseEvent => A): Reaction[A] =
    on[MouseEvent, A]("dblclick")(handler)
  @inline final def click[A](handler: MouseEvent => A): Reaction[A] =
    on[MouseEvent, A]("click")(handler)
}

/** Namespace of the tag node, either HTML or SVG */
sealed abstract class Namespace(val uri: String)
object Namespace {
  case object HTML extends Namespace("http://www.w3.org/1999/xhtml")
  case object SVG extends Namespace("http://www.w3.org/2000/svg")
}

/** Represents an HTML/SVG tree whose reactions produce values of type A*/
sealed abstract class Html[+A] {
  def map[B](f: A => B): Html[B]

  @inline final def postProcessing(effect: Node => Node): Html[A] =
    Html.PostProcessing(this, effect)
}

object Html {

  /** Represents a texte node */
  final case class Text(value: String) extends Html[Nothing] {
    def map[B](f: Nothing => B): Html[B] = this
    override def toString: String = value
  }

  /** Represents a tag node */
  sealed abstract case class Tag[+A](
      namespace: Namespace,
      tag: String,
      attributes: Map[Attribute.Key, Attribute.Value],
      reactions: Seq[Reaction[A]],
      children: List[Html[A]]
  ) extends Html[A] {

    @inline final def map[B](f: A => B): Html[B] =
      Tag(
        namespace,
        tag,
        attributes,
        reactions.map(_.map(f)),
        children.map(_.map(f))
      )
  }
  object Tag {

    /** Dirty but makes the compiler happy */
    private final class Tag[+A](
        namespace: Namespace,
        tag: String,
        attributes: Map[Attribute.Key, Attribute.Value],
        reactions: Seq[Reaction[A]],
        children: List[Html[A]]
    ) extends Html.Tag[A](namespace, tag, attributes, reactions, children)

    def apply[A](
        namespace: Namespace,
        tag: String,
        attributes: Map[Attribute.Key, Attribute.Value],
        reactions: Seq[Reaction[A]],
        children: List[Html[A]]
    ): Html.Tag[A] = {

      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      def reduceChildren(l: List[Html[A]]): List[Html[A]] =
        l match {
          case Nil                       => Nil
          case Text("") :: q             => q
          case Text(s1) :: Text(s2) :: q => reduceChildren(Text(s1 + s2) :: q)
          case tag :: q                  => tag :: reduceChildren(q)
        }

      new Tag[A](
        namespace,
        tag,
        attributes,
        reactions,
        reduceChildren(children)
      )
    }
  }

  final case class PostProcessing[+A](html: Html[A], effect: Node => Node)
      extends Html[A] {
    def map[B](f: A => B): Html[B] = PostProcessing(html.map(f), effect)
  }
}
