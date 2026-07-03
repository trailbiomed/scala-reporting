package trail.reporting

import com.github.plokhotnyuk.jsoniter_scala.core.*
import trail.reporting.schema.*
import trail.reporting.schema.Codecs.given

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.Base64

object Report {

  def write(document: Document, output: Path): Unit = {
    val bytes = render(document).getBytes(StandardCharsets.UTF_8)
    val _     = Files.write(output, bytes)
  }

  def render(document: Document): String = {
    val json       = writeToString(document)
    val safeJson   = json.replace("</", "<\\/")
    val bundle     = readResource("trail-reporting/browser.js")
    val safeBundle = bundle.replace("</", "<\\/")
    val title      = htmlEscape(document.title)
    val bioPvTag   =
      if (containsPdb(document)) {
        val src = readResource("trail-reporting/bio-pv.min.js").replace("</", "<\\/")
        s"<script>$src</script>"
      } else ""
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |<meta charset="utf-8">
       |<meta name="viewport" content="width=device-width, initial-scale=1">
       |<title>$title</title>
       |</head>
       |<body style="margin:0;font-family:ui-sans-serif,system-ui,-apple-system,'Segoe UI',Roboto,sans-serif">
       |<div id="trail-report-root"></div>
       |<script id="trail-report-data" type="application/json">$safeJson</script>
       |$bioPvTag
       |<script>$safeBundle</script>
       |</body>
       |</html>
       |""".stripMargin
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

  private def readResource(name: String): String = {
    val cl = Thread.currentThread.getContextClassLoader match {
      case null => getClass.getClassLoader
      case ok   => ok
    }
    val is = cl.getResourceAsStream(name)
    require(
      is != null,
      s"$name not found on classpath. Run `sbt jvm/compile` to relink the Scala.js bundle."
    )
    try new String(is.readAllBytes(), StandardCharsets.UTF_8)
    finally is.close()
  }

  private def htmlEscape(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
}
