package typed
package web
package syntax

import org.scalajs.dom.{Event, HTMLInputElement, KeyboardEvent, MouseEvent}
import Html._

import scala.scalajs.js

/** Small DSL to write HTML/SVG trees as if it would actually be HTML/SVG */
@SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
object html:

  /** A parameter is either:
    *   - an attribute that will be attached to the corresponding node.
    *   - a reaction that will be added to the event listener of the given node.
    *   - Nop that has no effect on the produced HTML/SVG, but it is convenient syntactic wise.
    */
  enum Parameter[+A]:
    case Attr(attr: (Attribute.Key, Attribute.Value)) extends Parameter[Nothing]
    case Reac[+A](reac: Reaction[A])                  extends Parameter[A]
    case Nop                                          extends Parameter[Nothing]

  object Parameter:
    def toAttributes[A](p: Seq[Parameter[A]])(
        old: Map[Attribute.Key, Attribute.Value]
    ): Map[Attribute.Key, Attribute.Value] =
      p.foldLeft(old) { case (m, attr) =>
        import Attribute._

        attr match {
          case Attr((k, v)) if Key.mergeable.contains(k) =>
            m + (k ->
              (m.get(k) match {
                case Some(v2) =>
                  Attribute.Value(v2.value + " " + v.value)
                case _ =>
                  v
              }))

          case Attr(kv) =>
            m + kv

          case _ =>
            m
        }
      }

  /** Node creation helper */
  def node_[A](
      namespace: Namespace,
      tag: String,
      ar: Seq[Parameter[A]],
      e: Seq[Html[A]]
  ): Tag[A] =

    val attributes: Map[Attribute.Key, Attribute.Value] =
      Parameter.toAttributes(ar)(Map.empty[Attribute.Key, Attribute.Value])

    val reactions: Seq[Reaction[A]] =
      ar.flatMap {
        case Parameter.Reac(r) => List(r)
        case _                 => Nil
      }

    Tag(namespace, tag, attributes, reactions, e.toList)

  inline def node[A](tag: String, namespace: Namespace = Namespace.HTML)(
      ar: Parameter[A]*
  )(e: Html[A]*): Tag[A] =
    node_(namespace, tag, ar, e)

  ////////////////////////////////////////////////////////////////
  //  HTML Namespace Tags
  //

  inline final def a[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("a")(ar*)(e*)
  inline final def blockquote[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("blockquote")(ar*)(e*)
  inline final def br[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("br")(ar*)(e*)
  inline final def button[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("button")(ar*)(e*)
  inline final def code[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("code")(ar*)(e*)
  inline final def div[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("div")(ar*)(e*)
  inline final def em[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("em")(ar*)(e*)
  inline final def h1[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("h1")(ar*)(e*)
  inline final def h2[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("h2")(ar*)(e*)
  inline final def h3[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("h3")(ar*)(e*)
  inline final def h4[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("h4")(ar*)(e*)
  inline final def img[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("img")(ar*)(e*)
  inline final def input[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("input")(ar*)(e*)
  inline final def li[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("li")(ar*)(e*)
  inline final def ol[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("ol")(ar*)(e*)
  inline final def p[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("p")(ar*)(e*)
  inline final def pre[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("pre")(ar*)(e*)
  inline final def q[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("q")(ar*)(e*)
  inline final def small[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("small")(ar*)(e*)
  inline final def span[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("span")(ar*)(e*)
  inline final def strong[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("strong")(ar*)(e*)
  inline final def styleTag[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("style")(ar*)(e*)
  inline final def text(s: String): Text =
    Text(s)
  inline final def ul[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("ul")(ar*)(e*)
  inline final def video[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("video")(ar*)(e*)

  ////////////////////////////////////////////////////////////////
  // SVG Namespace Tags
  //

  inline final def circle[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("circle", Namespace.SVG)(ar*)(e*)
  inline final def defs[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("defs", Namespace.SVG)(ar*)(e*)
  inline final def g[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("g", Namespace.SVG)(ar*)(e*)
  inline final def line[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("line", Namespace.SVG)(ar*)(e*)
  inline final def polygon[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("polygon", Namespace.SVG)(ar*)(e*)
  inline final def polyline[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("polyline", Namespace.SVG)(ar*)(e*)
  inline final def rect[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("rect", Namespace.SVG)(ar*)(e*)
  inline final def svg[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("svg", Namespace.SVG)(ar*)(e*)
  inline final def symbol[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("symbol", Namespace.SVG)(ar*)(e*)
  inline final def svgText[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("text", Namespace.SVG)(ar*)(e*)
  inline final def use[A](ar: Parameter[A]*)(e: Html[A]*): Tag[A] =
    node[A]("use", Namespace.SVG)(ar*)(e*)

  ////////////////////////////////////////////////////////////////
  // Static Paramaters
  //

  val nop: Parameter[Nothing] = Parameter.Nop

  /** Type of attribute builders */
  type MakeAttr = String => Parameter[Nothing]

  inline def attrOfKey(key: Attribute.Key): MakeAttr =
    v => Parameter.Attr((key, Attribute.Value(v)))

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  inline def attr(clef: String, namespace: String = ""): MakeAttr =
    val ns: Option[Attribute.Namespace] =
      if namespace.isEmpty
      then None
      else Some(Attribute.Namespace(namespace))

    attrOfKey(Attribute.Key(clef, ns))

  inline final def alt: MakeAttr          = attr("alt")
  inline def autoplay: Parameter[Nothing] = attr("autoplay")("")
  inline final def checked(b: Boolean): Parameter[Nothing] =
    if b
    then attr("checked")("checked")
    else Parameter.Nop
  inline final def cite: MakeAttr               = attr("cite")
  inline final def `class`: MakeAttr            = attrOfKey(Attribute.Key.`class`)
  inline final def controls: Parameter[Nothing] = attr("controls")("")
  inline final def cx: MakeAttr                 = attr("cx")
  inline final def cy: MakeAttr                 = attr("cy")
  inline final def id: MakeAttr                 = attr("id")
  inline final def fill: MakeAttr               = attr("fill")
  inline final def height: MakeAttr             = attr("height")
  inline final def href: MakeAttr               = attr("href")
  inline final def loop(b: Boolean): Parameter[Nothing] =
    attr("loop")(if b then "true" else "false")
  inline final def media: MakeAttr = attr("media")
  inline final def muted: Parameter[Nothing] =
    attr("muted")("")
  inline final def playsinline: Parameter[Nothing] =
    attr("playsinline")("")
  inline final def points: MakeAttr              = attr("points")
  inline final def preserveAspectRatio: MakeAttr = attr("preserveAspectRatio")
  inline final def r: MakeAttr                   = attr("r")
  inline final def rel: MakeAttr                 = attr("rel")
  inline final def src: MakeAttr                 = attr("src")
  inline final def stroke: MakeAttr              = attr("stroke")
  inline final def strokeWidth: MakeAttr         = attr("stroke-width")
  inline final def style: MakeAttr               = attrOfKey(Attribute.Key.style)
  inline final def transform: MakeAttr           = attr("transform")
  inline final def `type`: MakeAttr              = attr("type")
  inline final def value: MakeAttr               = attr("value")
  inline final def viewBox: MakeAttr             = attr("viewBox")
  inline final def width: MakeAttr               = attr("width")
  inline final def x: MakeAttr                   = attr("x")
  inline final def x1: MakeAttr                  = attr("x1")
  inline final def x2: MakeAttr                  = attr("x2")
  inline final def xlinkHref: MakeAttr =
    attr("xlink:href", "http://www.w3.org/1999/xlink")
  inline final def xmlns: MakeAttr = attr("xmlns")
  inline final def y: MakeAttr     = attr("y")
  inline final def y1: MakeAttr    = attr("y1")
  inline final def y2: MakeAttr    = attr("y2")

  ////////////////////////////////////////////////////////////////
  // Event Paramaters
  //

  /** Type of reaction builders */
  type MakeReaction[A] = js.Function1[? <: Event, A] => Parameter[A]

  inline def on[T <: Event, A](
      `type`: String
  )(f: js.Function1[T, A]): Parameter[A] =
    Parameter.Reac(Reaction.on[T, A](`type`)(f))

  inline def on0[A](`type`: String)(msg: => A): Parameter[A] =
    on[Event, A](`type`) { _ => msg }

  inline final def onsubmit[A](msg: => A): Parameter[A] = on0("submit")(msg)
  inline final def onclick[A](msg: => A): Parameter[A]  = on0("click")(msg)

  inline final def onkeyup[A](handler: KeyboardEvent => A): Parameter[A] =
    Parameter.Reac(Reaction.keyup(handler))
  inline final def onkeydown[A](handler: KeyboardEvent => A): Parameter[A] =
    Parameter.Reac(Reaction.keydown(handler))
  inline final def onkeypress[A](handler: KeyboardEvent => A): Parameter[A] =
    Parameter.Reac(Reaction.keypress(handler))

  inline final def onmousedown[A](handler: MouseEvent => A): Parameter[A] =
    Parameter.Reac(Reaction.mousedown(handler))
  inline final def onmouseup[A](handler: MouseEvent => A): Parameter[A] =
    Parameter.Reac(Reaction.mouseup(handler))
  inline final def ondblclick[A](handler: MouseEvent => A): Parameter[A] =
    Parameter.Reac(Reaction.dblclick(handler))
  inline def onInputElement[A](ext: HTMLInputElement => A): Parameter[A] =
    on[Event, A]("input") { (e: Event) =>
      e.target match {
        case input: HTMLInputElement =>
          ext(input)
      }
    }

  inline def oninput[A](reaction: String => A): Parameter[A] =
    onInputElement[A](i => reaction(i.value))

  inline def oncheck[A](reaction: Boolean => A): Parameter[A] =
    onInputElement[A](i => reaction(i.checked))

  extension [A](self: Html[A])
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def add(params: Parameter[A]*)(newChildren: Html[A]*): Html[A] =
      self match
        case Html.Text(s) =>
          span(params*)(text(s) +: newChildren*)

        case Html.Tag(namespace, tag, attributes, reactions, children) =>
          val newAttributes: Map[Attribute.Key, Attribute.Value] =
            Parameter.toAttributes(params)(attributes)

          val newReactions: Seq[Reaction[A]] =
            params.flatMap {
              case Parameter.Reac(r) => List(r)
              case _                 => Nil
            }

          Tag(
            namespace,
            tag,
            newAttributes,
            reactions ++ newReactions,
            children ++ newChildren.toList
          )

        case Html.PostProcessing(html, effect) =>
          Html.PostProcessing(html.add(params*)(newChildren*), effect)
