package typed
package web

import org.scalajs._
import org.scalajs.dom._
import org.scalajs.dom.raw._

/** A Web Application.
  * Just like a Text Application but the view return an Html tree
  */
trait WebApp extends run.Application { self =>

  /** The view returns an Html tree */
  def view(model: Model): Html[Option[Msg]]

  /** The view returns an Html tree */
  def documentReactions(model: Model): List[Reaction[Option[Msg]]]

  /** Run a Web Application on a given node
    *
    * The principle is simple:
    *  - Just like any {{{Application}}}, at any given time there is a
    *    "current model" which is the state of the application, a value of type [[Model]].
    *  - The function [[view]] compute the Html tree corresponding the current value of the model.
    *  - This tree enables building a Node of the DOM
    *  - This new Node replaces the old one.
    *  - Each event (thus any reaction executed) returns a un message (of type [[Msg]]).
    *  - This message updates the state of the application via the [[update]] function.
    *  - The new state becomes the new current state and we loop.
    */
  @SuppressWarnings(
    Array(
      "org.wartremover.warts.Var",
      "org.wartremover.warts.NonUnitStatements",
      "org.wartremover.warts.Recursion"
    )
  )
  final def run(
      initialNode: Node,
      differenceActivated: WebApp.DifferenceActivated
  ): Unit = {
    final case class State(
        node: Node,
        view: Html[Unit],
        model: Model,
        reactions: List[Reaction[Unit]]
    )

    var state: State =
      State(initialNode, Html.Text(""), initialModel, Nil)

    def actualize(newModel: Model): Unit = {
      // Diabling old document reactions
      state.reactions.foreach { r =>
        document.removeEventListener(r.`type`, r.reaction, false)
      }

      // Computing new view
      val newView: Html[Unit] =
        view(newModel).map {
          case Some(msg) => actualize(update(msg, state.model))
          case _         => ()
        }

      val oldNode: Node = state.node
      val parent: Node = oldNode.parentNode

      // Adding new document reactions
      val newDocumentReactions: List[Reaction[Unit]] =
        documentReactions(newModel).map { react: Reaction[Option[Msg]] =>
          react.map {
            case Some(msg) => actualize(update(msg, state.model))
            case _         => ()
          }
        }

      window.requestAnimationFrame { _ =>
        val newNode: Node =
          if (differenceActivated.boolean)
            Rendering.difference(
              parent,
              Rendering.Entry(state.view, state.node),
              newView
            )
          else {
            val n = Rendering.draw(newView)
            parent.replaceChild(n, oldNode)
            n
          }

        newDocumentReactions.foreach { r =>
          document.addEventListener(r.`type`, r.reaction, false)
        }

        // Saving state
        state = State(newNode, newView, newModel, newDocumentReactions)
      }

      ()
    }

    actualize(initialModel)
  }

  /** Run the application on a node
    *
    * @param id attribute of the node
    */
  @inline final def runMain(
      id: String,
      differenceActivated: WebApp.DifferenceActivated
  ): Unit =
    WebApp.onLoading {
      run(document.getElementById(id), differenceActivated)
    }
}

object WebApp {
  sealed abstract class DifferenceActivated(val boolean: Boolean)
  final case object UseDifference extends DifferenceActivated(true)
  final case object RedrawEverything extends DifferenceActivated(false)

  @SuppressWarnings(Array("org.wartremover.warts.Nothing"))
  def onLoading(a: => Unit): Unit =
    org.scalajs.dom.document
      .addEventListener("DOMContentLoaded", (_: Event) => a)

  @SuppressWarnings(
    Array(
      "org.wartremover.warts.AsInstanceOf",
      "org.wartremover.warts.NonUnitStatements"
    )
  )
  def download(blob: Blob, name: String): Unit = {
    val a = document
      .createElementNS(Namespace.HTML.uri, "a")
      .asInstanceOf[HTMLAnchorElement]
    val url = dom.raw.URL.createObjectURL(blob)
    a.href = url
    a.rel = "noopener"
    a.setAttribute("download", name)
    a.style = "display: none;"
    document.body.appendChild(a)
    a.click()
    document.body.removeChild(a)
    dom.raw.URL.revokeObjectURL(url)
  }
}
