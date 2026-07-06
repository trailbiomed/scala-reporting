package trail.reporting

import com.github.plokhotnyuk.jsoniter_scala.core.*
import trail.reporting.schema.*
import trail.reporting.schema.Codecs.given

import java.io.{BufferedOutputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.Base64

object Report {

  def write(document: Document, output: Path): Unit = {
    val out = Files.newOutputStream(output)
    try render(document, out)
    finally out.close()
  }

  def render(document: Document, out: OutputStream): Unit = {
    val buffered = out match {
      case b: BufferedOutputStream => b
      case _                       => new BufferedOutputStream(out, 64 * 1024)
    }
    val hasPdb = containsPdb(document)
    val title  = htmlEscape(document.title)

    val header =
      s"""<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |<meta charset="utf-8">
         |<meta name="viewport" content="width=device-width, initial-scale=1">
         |<title>$title</title>
         |</head>
         |<body style="margin:0;font-family:ui-sans-serif,system-ui,-apple-system,'Segoe UI',Roboto,sans-serif">
         |<div id="trail-report-root"></div>
         |<script id="trail-report-data" type="application/json">""".stripMargin
    writeUtf8(buffered, header)

    writeToStream(document, new HtmlScriptEscapingOutputStream(buffered))
    writeUtf8(buffered, "</script>\n")

    if (hasPdb) {
      writeUtf8(buffered, "<script>")
      streamResource("trail-reporting/bio-pv.min.js", new HtmlScriptEscapingOutputStream(buffered))
      writeUtf8(buffered, "</script>\n")
    } else {
      writeUtf8(buffered, "\n")
    }

    writeUtf8(buffered, "<script>")
    streamResource("trail-reporting/browser.js", new HtmlScriptEscapingOutputStream(buffered))
    writeUtf8(buffered, "</script>\n</body>\n</html>\n")

    buffered.flush()
  }

  def render(document: Document): String = {
    val bos = new ByteArrayOutputStream(64 * 1024)
    render(document, bos)
    bos.toString(StandardCharsets.UTF_8)
  }

  private def containsPdb(document: Document): Boolean =
    document.pages.exists(_.items.exists(_.data.exists {
      case _: DataItem.PdbItem => true
      case _                   => false
    }))

  def sourceFromPath(path: Path): SourceFile = {
    val bytes = Files.readAllBytes(path)
    val mime  = Option(Files.probeContentType(path)).getOrElse("application/octet-stream")
    SourceFile(
      filename      = path.getFileName.toString,
      mimeType      = mime,
      contentBase64 = Base64.getEncoder.encodeToString(bytes)
    )
  }

  def nowIso(): String = Instant.now().toString

  private def writeUtf8(out: OutputStream, s: String): Unit =
    out.write(s.getBytes(StandardCharsets.UTF_8))

  private def streamResource(name: String, out: OutputStream): Unit = {
    val cl = Thread.currentThread.getContextClassLoader match {
      case null => getClass.getClassLoader
      case ok   => ok
    }
    val is = cl.getResourceAsStream(name)
    require(
      is != null,
      s"$name not found on classpath. Run `sbt jvm/compile` to relink the Scala.js bundle."
    )
    try copyStream(is, out)
    finally is.close()
  }

  private def copyStream(in: InputStream, out: OutputStream): Unit = {
    val buf = new Array[Byte](8192)
    var n   = in.read(buf)
    while (n > 0) {
      out.write(buf, 0, n)
      n = in.read(buf)
    }
  }

  private def htmlEscape(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  /** Filter stream that turns every `</` byte sequence into `<\/` so the enclosed content is safe to embed inside a `<script>` element. */
  private final class HtmlScriptEscapingOutputStream(underlying: OutputStream) extends OutputStream {
    private val Lt: Byte    = '<'.toByte
    private val Slash: Byte = '/'.toByte
    private val Bsl: Int    = '\\'.toInt
    private var lastWasLt   = false

    override def write(b: Int): Unit = {
      val byte = (b & 0xFF).toByte
      if (lastWasLt && byte == Slash) underlying.write(Bsl)
      underlying.write(byte.toInt & 0xFF)
      lastWasLt = byte == Lt
    }

    override def write(buf: Array[Byte], off: Int, len: Int): Unit = {
      val end     = off + len
      var runFrom = off
      var i       = off
      while (i < end) {
        val b = buf(i)
        if (lastWasLt && b == Slash) {
          if (runFrom < i) underlying.write(buf, runFrom, i - runFrom)
          underlying.write(Bsl)
          runFrom = i
        }
        lastWasLt = b == Lt
        i += 1
      }
      if (runFrom < end) underlying.write(buf, runFrom, end - runFrom)
    }

    override def flush(): Unit = underlying.flush()
    override def close(): Unit = ()
  }
}
