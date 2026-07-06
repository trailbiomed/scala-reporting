package trail.reporting.browser

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.raquo.laminar.api.L.{Mod as _, *}
import com.raquo.laminar.nodes.RootNode
import org.scalajs.dom
import trail.reporting.schema.*
import trail.reporting.schema.Codecs.given
import lui.style.*

def mount(mountEl: dom.Element, document: Document): RootNode = {
  reset.install()
  Theme.signal.foreach { t =>
    dom.document.body.style.backgroundColor = t.bg.toCss
    dom.document.body.style.color = t.text.toCss
  }(unsafeWindowOwner)
  render(mountEl, App(document).root)
}

/** Convenience: parse a JSON string encoding of [[Document]] and mount it. */
def mount(mountEl: dom.Element, documentJson: String): RootNode =
  mount(mountEl, readFromString[Document](documentJson))

@main def main(): Unit = {
  val mountEl = dom.document.getElementById("trail-report-root")
  if (mountEl == null) {
    dom.console.error("trail-report-root mount point not found")
  } else {
    val dataTag = dom.document.getElementById("trail-report-data")
    val doc =
      if (dataTag == null) emptyDocument("Missing data payload")
      else
        try readFromString[Document](dataTag.textContent)
        catch {
          case t: Throwable =>
            dom.console.error(s"Failed to decode report payload: ${t.getMessage}")
            emptyDocument(s"Decode error: ${t.getMessage}")
        }
    val _ = mount(mountEl, doc)
  }
}

private def emptyDocument(reason: String): Document =
  Document(
    title     = "Report",
    pages     = Seq(
      Page(
        id    = "error",
        title = "Error",
        items = Seq(
          Item(
            id    = "error",
            title = "Could not load report",
            data  = Seq(DataItem.TextItem(reason))
          )
        )
      )
    ),
    createdAt = "",
    version   = None,
    source    = None
  )
