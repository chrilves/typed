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
  def view(model: Model): Html[Msg]

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
    Array("org.wartremover.warts.Var",
          "org.wartremover.warts.NonUnitStatements",
          "org.wartremover.warts.Recursion"))
  final def run(initialNode: Node): Unit = {
    import Html._

    final case class State(node: Node, view: Html[Unit], model: Model)

    var state: State =
      State(initialNode, Text(""), initialModel)

    def actualize(newModel: Model): Unit = {
      val newView: Html[Unit] =
        view(newModel).map { msg: Msg =>
          actualize(update(msg, state.model))
        }

      val oldNode: Node = state.node
      val parent: Node = oldNode.parentNode

      val newNode: Node = {
        val n = newView.draw
        parent.replaceChild(n, oldNode)
        n
      }

      state = State(newNode, newView, newModel)
    }

    actualize(initialModel)
  }

  /** Run the application on a node
    *
    * @param id attribute of the node
    */
  @inline final def runMain(id: String): Unit =
    WebApp.onLoading {
      run(document.getElementById(id))
    }
}

object WebApp {
  @SuppressWarnings(Array("org.wartremover.warts.Nothing"))
  def onLoading(a: => Unit): Unit =
    org.scalajs.dom.document
      .addEventListener("DOMContentLoaded", (_: Event) => a)

  @SuppressWarnings(
    Array("org.wartremover.warts.AsInstanceOf",
          "org.wartremover.warts.NonUnitStatements"))
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
