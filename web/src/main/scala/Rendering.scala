package typed
package web

import org.scalajs.dom
import dom.document
import org.scalajs.dom.raw._
import org.scalajs.dom.ext._
import typed.algorithms.Diff

sealed abstract class NodeType
object NodeType {
  final case class Text(value: String) extends NodeType
  final case class AnonymousTag(space: Namespace, tag: String) extends NodeType
  final case class NamedTag(space: Namespace, tag: String, id: Attribute.Value)
      extends NodeType
  final case object PostProcessed extends NodeType

  def apply[A](html: Html[A]): NodeType =
    html match {
      case Html.Text(s) =>
        Text(s)
      case Html.Tag(namespace, tag, attrs, _, _) =>
        attrs.get(Attribute.Key("id", None)) match {
          case Some(id) => NamedTag(namespace, tag, id)
          case None     => AnonymousTag(namespace, tag)
        }
      case Html.PostProcessing(_, _) => PostProcessed
    }
}

object Rendering {
  final case class Entry(html: Html[Unit], node: Node)
  import Html._

  @SuppressWarnings(
    Array("org.wartremover.warts.Null", "org.wartremover.warts.Recursion")
  )
  def draw(h: Html[Unit]): Node =
    h match {
      case Text(s) =>
        document.createTextNode(s)

      case Tag(space, tag, attributs, reactions, children) =>
        val b: Element = document.createElementNS(space.uri, tag)

        attributs.foreach {
          case (Attribute.Key(clef, ns), Attribute.Value(value)) =>
            b.setAttributeNS(ns.map(_.value).getOrElse(null), clef, value)
        }

        reactions.foreach {
          case Reaction(t, r) =>
            b.addEventListener(t, r, false)
        }

        children.foreach { enfant => b.appendChild(draw(enfant)) }

        b

      case PostProcessing(html, effect) =>
        effect(draw(html))
    }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def update(
      node: Element,
      oldAttribtes: Map[Attribute.Key, Attribute.Value],
      oldReactions: Seq[Reaction[Unit]],
      newAttributes: Map[Attribute.Key, Attribute.Value],
      newReactions: Seq[Reaction[Unit]]
  ): Unit = {

    // Removal of old reactions
    oldReactions.foreach {
      case Reaction(t, r) =>
        node.removeEventListener(t, r, false)
    }

    // Setup of new reactions
    newReactions.foreach {
      case Reaction(t, r) =>
        node.addEventListener(t, r, false)
    }

    // Removed attributes
    (oldAttribtes.keys.toSet -- newAttributes.keys).foreach {
      case Attribute.Key(clef, ns) =>
        node.removeAttributeNS(ns.map(_.value).getOrElse(null), clef)
    }

    // Updated and added Attributes
    newAttributes.foreach {
      case (c @ Attribute.Key(clef, ns), v @ Attribute.Value(value)) =>
        oldAttribtes.get(c) match {
          case Some(av) if av === v =>
            ()
          case Some(_) =>
            node.setAttributeNS(ns.map(_.value).getOrElse(null), clef, value)
        }
    }
  }

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  def difference(
      parent: Node,
      old: Entry,
      newHtml: Html[Unit]
  ): Node =
    newHtml match {
      case Text(s) =>
        old match {
          case Entry(Text(u), text: dom.raw.Text) =>
            if (u =/= s) text.textContent = s
            text
          case _ =>
            val newNode = document.createTextNode(s)
            parent.replaceChild(newNode, old.node)
            newNode
        }

      case Tag(namespace, tag, attributes, reactions, newChildren) =>
        old match {
          case Entry(
              Tag(a_namespace, a_tag, a_attrs, a_reactions, oldChildren),
              oldElements: dom.raw.Element
              ) if a_tag === tag && a_namespace === namespace =>
            update(oldElements, a_attrs, a_reactions, attributes, reactions)

            val oldEntries: Array[Entry] =
              oldChildren.toArray
                .zip(oldElements.childNodes)
                .map { case (h, n) => Entry(h, n) }

            def sameNodeType(g: Entry, d: Html[Unit]): Boolean =
              NodeType(g.html) === NodeType(d)

            val diffs: List[Diff[Entry, Html[Unit]]] =
              Diff
                .myers(sameNodeType _)(oldEntries, newChildren.toArray)
                ._2

            import Diff._

            @scala.annotation.tailrec
            def applyDiffs(
                last: Option[Node],
                l: List[Diff[Entry, Html[Unit]]]
            ): Unit =
              l match {
                case Nil =>
                  ()

                case Deletion(a) :: Insertion(b) :: tl =>
                  applyDiffs(last, Replacement(a, b) :: tl)

                case Replacement(entry, h2) :: tl =>
                  val newOne = draw(h2)
                  val newLast =
                    old.node.replaceChild(newOne, entry.node)
                  applyDiffs(Some(newLast), tl)

                case Deletion(entree) :: tl =>
                  old.node.removeChild(entree.node)
                  applyDiffs(last, tl)

                case Insertion(h2) :: tl =>
                  val newOne = draw(h2)
                  val newLast =
                    last match {
                      case Some(n) =>
                        old.node.insertBefore(newOne, n)
                      case _ =>
                        old.node.appendChild(newOne)
                    }

                  applyDiffs(Some(newLast), tl)

                case Identical(entry, h2) :: tl =>
                  val newLast = difference(old.node, entry, h2)
                  applyDiffs(Some(newLast), tl)
              }
            applyDiffs(None, diffs.reverse)
            old.node

          case _ =>
            val newNode = draw(newHtml)
            parent.replaceChild(newNode, old.node)
            newNode
        }

      case PostProcessing(html, effect) =>
        val newNode = effect(draw(html))
        parent.replaceChild(newNode, old.node)
        newNode
    }
}
