package trail.reporting.browser

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.raquo.laminar.api.L.{Mod as _, *}
import org.scalajs.dom
import trail.reporting.schema.*
import trail.reporting.schema.Codecs.given
import lui.style.*

@main def main(): Unit = {
  reset.install()
  val mount = dom.document.getElementById("trail-report-root")
  if (mount == null) {
    dom.console.error("trail-report-root mount point not found")
  } else {
    Theme.signal.foreach { t =>
      dom.document.body.style.backgroundColor = t.bg.toCss
      dom.document.body.style.color = t.text.toCss
    }(unsafeWindowOwner)

    val doc = readDocument()
    val _ = render(mount, App(doc).root)
  }
}

private def readDocument(): Document = {
  val tag = dom.document.getElementById("trail-report-data")
  if (tag == null) emptyDocument("Missing data payload")
  else {
    try readFromString[Document](tag.textContent)
    catch {
      case t: Throwable =>
        dom.console.error(s"Failed to decode report payload: ${t.getMessage}")
        emptyDocument(s"Decode error: ${t.getMessage}")
    }
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
