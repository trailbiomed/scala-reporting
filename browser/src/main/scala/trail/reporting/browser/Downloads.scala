package trail.reporting.browser

import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

object Downloads {

  def fromBase64(base64: String, filename: String, mime: String): Unit = {
    val raw   = dom.window.atob(base64)
    val bytes = new Uint8Array(raw.length)
    var i = 0
    while (i < raw.length) {
      bytes(i) = (raw.charAt(i).toInt & 0xff).toShort
      i += 1
    }
    val blob = new dom.Blob(js.Array(bytes), new dom.BlobPropertyBag { `type` = mime })
    triggerDownload(dom.URL.createObjectURL(blob), filename)
  }

  def fromText(text: String, filename: String, mime: String): Unit = {
    val blob = new dom.Blob(js.Array(text), new dom.BlobPropertyBag { `type` = mime })
    triggerDownload(dom.URL.createObjectURL(blob), filename)
  }

  private def triggerDownload(url: String, filename: String): Unit = {
    val a = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
    a.href = url
    a.setAttribute("download", filename)
    a.style.display = "none"
    val _b = dom.document.body.appendChild(a)
    a.click()
    val _r = dom.document.body.removeChild(a)
    val _t = js.timers.setTimeout(0)(dom.URL.revokeObjectURL(url))
  }
}
