package trail.reporting.example

import org.nspl.*
import org.nspl.awtrenderer.*
import org.saddle.{Frame, Series, Vec, Index}
import trail.reporting.{Report, SaddleAdapter}
import trail.reporting.dsl.*
import trail.reporting.schema.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object Main {

  def main(args: Array[String]): Unit = {
    val output = args.headOption.map(Paths.get(_)).getOrElse(Paths.get("report.html"))

    val n        = 240
    val rowIx    = Index(Array.range(1, n + 1))
    val scoreVec = Vec(Array.tabulate(n)(i => math.sin((i + 1) * 0.4) * 50 + 50))
    val flagVec  = Vec(Array.tabulate(n)(i => (i + 1) % 3 == 0))
    val metrics  = Frame("score" -> Series(rowIx, scoreVec))

    val cohortTable = TableSpec(
      Seq(
        IntegerColumn("id",      "ID",      (1 to n).map(_.toLong)),
        StringColumn("sample",  "Sample",   (1 to n).map(i => f"S$i%03d")),
        SaddleAdapter.numberColumn("score", "Score", metrics.firstCol("score")),
        SaddleAdapter.boolColumn("flagged", "Flagged", Series(rowIx, flagVec)),
        StringColumn(
          "notes",
          "Notes",
          (1 to n).map(i => if (i % 7 == 0) "" else s"row $i"),
          nulls = (1 to n).iterator.collect { case i if i % 7 == 0 => i - 1 }.toSet
        )
      )
    )

    val xs        = (0 to 100).map(_.toDouble * 0.1)
    val sinSeries = xs.map(x => (x, math.sin(x)))
    val cosSeries = xs.map(x => (x, math.cos(x)))
    val plotBuild =
      xyplot(sinSeries -> line(), cosSeries -> line())(
        par.xlab("x").ylab("y").main("sin / cos")
      )

    val sourceCandidate = Paths.get(
      sys.props.getOrElse("user.dir", "."),
      "example/src/main/scala/trail/reporting/example/Main.scala"
    )

    val logoSvg =
      """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round">
        |  <path d="M6 6h20"/>
        |  <path d="M16 6v20"/>
        |  <path d="M10 22l6 4 6-4"/>
        |</svg>""".stripMargin

    val baseDoc = document("Sample Report")
      .withVersion("0.2.0")
      .withLogo(logoSvg)
      .withFootnote("Confidential — Trail Biomed. For internal use only.")
    val withSrc =
      if (Files.isRegularFile(sourceCandidate)) baseDoc.withSource(sourceCandidate) else baseDoc

    val doc = withSrc
      .verticalNavigator
      .page(
        page("overview", "Overview",
          item("intro", "Introduction")
            .text(
              "This is a self-contained sample report. Data is embedded as JSON in this " +
                "single HTML file and the Scala.js bundle that renders it is inlined too."
            )
            .code(
              "scala",
              """val doc = document("Title")
                |  .withVersion("0.2.0")
                |  .withSource(path)
                |  .verticalNavigator
                |  .page("overview", "Overview",
                |    item("metrics", "Cohort").text("...").frame(metricsFrame)
                |  )
                |Report.write(doc, Paths.get("report.html"))""".stripMargin
            ),
          item("formatting", "Text formatting")
            .text(
              """`.text(...)` accepts a minimal Markdown flavor. Supported constructs:
                |
                |- Emphasis: **bold**, *italic*, `inline code`, [links](https://example.com)
                |- Bullet lists (`- ` or `* ` at line start)
                |- ATX headings (`#`, `##`, `###`)
                |- Fenced code blocks with an optional language tag
                |
                |### Nested example
                |
                |A paragraph can mix **bold** with *italic* and `code`, and continue over
                |several source lines that collapse into a single paragraph.
                |
                |```scala
                |item("x", "X").text("**hello** *world*")
                |```
                |
                |Unmatched delimiters like a lone * render literally.""".stripMargin
            ),
          item("metrics", "Cohort metrics")
            .text(
              s"$n rows. The Score column is sourced via `SaddleAdapter.numberColumn` from a " +
                "Saddle Series; the rest are columnar values directly."
            )
            .table(cohortTable),
          item("metrics-frame", "Saddle frame via .frame")
            .text("Same Score column, but constructed via the .frame extension on a Frame[Int, String, Double].")
            .frame(metrics)
        ).withName("Section 01")
          .withTags("rows" -> n.toString, "kind" -> "tables")
          .withDescription(
            "Introduces the report itself and a synthetic 240-row cohort. Skim the intro, " +
              "then move into the metrics tables to see both column-major and Saddle-frame sources."
          )
      )
      .page(
        page("plots", "Plots",
          item("trig", "Trigonometric series")
            .text("SVG plot pre-rendered on the JVM via nspl-awt; the browser just embeds the SVG.")
            .plot(plotBuild, width = 800)
        ).withName("Section 02")
          .withTags("kind" -> "plot", "count" -> "1")
          .withItemMenu(PageItemMenu.Hidden)
          .withDescription(
            "Plots are rendered on the JVM at build time (nspl-awt) and embedded as SVG in the HTML."
          )
      )
      .page(
        page("tables", "Tables",
          item("small", "Small table fills the slide")
            .text(
              "Only **three rows** and three columns. In slideshow mode the font scales " +
                "up so the table fills the available space instead of sitting tiny in the corner."
            )
            .table(TableSpec(Seq(
              StringColumn("region", "Region", IndexedSeq("EMEA", "Americas", "APAC")),
              IntegerColumn("headcount", "Headcount", IndexedSeq(42L, 87L, 63L)),
              NumberColumn("share", "Share", IndexedSeq(0.234, 0.513, 0.253))
            ))),
          item("integers", "Thousand separators")
            .text(
              "Both `IntegerColumn` and `NumberColumn` add a non-breaking space between groups " +
                "of three digits when displayed — visual only, so search, sort, and CSV export still see " +
                "the raw digits. Hover any cell to see the unformatted value."
            )
            .table(TableSpec(Seq(
              StringColumn("account", "Account", IndexedSeq("Alpha", "Bravo", "Charlie", "Delta", "Echo")),
              IntegerColumn("balance",     "Balance",      IndexedSeq(1234567L, 82000L, 9500000L, 1000000000L, 45L)),
              IntegerColumn("transactions", "Transactions", IndexedSeq(1200L, 87L, 240000L, 3L, 15L)),
              IntegerColumn("delta",        "Delta",        IndexedSeq(-4200L, -87L, 12345L, 0L, -1500000L)),
              NumberColumn("revenue",       "Revenue",      IndexedSeq(1234567.89, 82000.5, 9500000.0, 1.2345e12, 45.678)),
              NumberColumn("ratio",         "Ratio",        IndexedSeq(0.234, 12345.6789, -98765.4321, 1000000.0, 0.00042))
            )))
        ).withName("Section 04")
          .withTags("kind" -> "tables", "cases" -> "2")
          .withDescription(
            "Slide-fit table scaling and integer thousand-separator formatting."
          )
      )
      .page(
        page("text-scaling", "Text scaling",
          item("tiny", "Short blurb grows")
            .text("A single sentence. Watch it fill the slide."),
          item("medium", "Medium-length section stays comfortable")
            .text(
              """Two or three paragraphs of prose — long enough that a huge font would overflow, short
                |enough that a tiny font would look silly. The heuristic picks a size in the middle.
                |
                |### Sub-heading
                |
                |Headings, `inline code`, **bold**, and *italic* all scale together because their sizes
                |are expressed in `em`, not `rem`. Only the body font-size responds to content length.""".stripMargin
            ),
          item("dense", "Dense content shrinks to fit")
            .text(
              """When a slide carries a lot of text, the body font shrinks so everything remains visible
                |without vertical scrolling. Headings and code blocks shrink proportionally.
                |
                |### Design notes
                |
                |- The scale bucket is picked from a stripped character count (markdown syntax, list
                |  bullets, and fenced code blocks are collapsed before measuring).
                |- Non-body sizes are `em`-relative so a single knob controls the whole slide.
                |- Buckets are coarse on purpose: five discrete sizes read as intentional layout, while
                |  a continuous scale would jitter between adjacent slides.
                |
                |### Where it applies
                |
                |Only the slideshow view uses this scaling. The regular page view keeps its normal
                |typography because reading a long report benefits from a stable, familiar text size —
                |there is no fixed viewport to fit into.
                |
                |```scala
                |// slideTextBodyRem picks one of:
                |//   3.000rem  (< 100 chars)
                |//   2.375rem  (< 240 chars)
                |//   1.875rem  (< 500 chars)  <- default
                |//   1.500rem  (< 1000 chars)
                |//   1.250rem  (< 2000 chars)
                |//   1.125rem  (else)
                |```
                |
                |That is roughly all there is to it. Open this slide fullscreen and compare it against
                |the *Short blurb grows* and *Medium-length section* items to see the three tiers in
                |action side by side.""".stripMargin
            )
        ).withName("Section 05")
          .withTags("kind" -> "text", "tiers" -> "3")
          .withDescription(
            "Slide text picks a body font-size from its content length. Compare the three items to " +
              "see short blurbs grow, medium sections stay comfortable, and dense content shrink."
          )
      )
      .page(
        page("structure", "Structure",
          item("crambin-cartoon", "Crambin (1CRN) — cartoon / ssSuccession")
            .text(
              "PDB structure rendered client-side with bio-pv. The PDB text is embedded in the report; " +
                "the WebGL viewer is spun up on mount, resizes with its container, and supports mouse rotate/zoom."
            )
            .pdb(loadPdbResource("1crn.pdb"), style = PdbStyle.Cartoon, color = PdbColor.SsSuccession, height = 480),
          item("crambin-spheres", "Crambin (1CRN) — spheres / byChain")
            .text("Same structure, different render + coloring — confirms two viewers coexist on one page.")
            .pdb(loadPdbResource("1crn.pdb"), style = PdbStyle.Spheres, color = PdbColor.ByChain, height = 480)
        ).withName("Section 03")
          .withTags("kind" -> "pdb", "pdb-id" -> "1CRN")
          .withItemMenu(PageItemMenu.Popover)
          .withDescription(
            "Client-side molecular structure viewers (bio-pv, WebGL). Two panes of Crambin (1CRN) " +
              "demonstrate independent style and coloring on the same page."
          )
      )

    Report.write(doc, output)
    println(s"Wrote ${output.toAbsolutePath} (${Files.size(output)} bytes)")
  }

  private def loadPdbResource(name: String): String = {
    val is = getClass.getClassLoader.getResourceAsStream(name)
    require(is != null, s"resource $name not on classpath")
    try new String(is.readAllBytes(), StandardCharsets.UTF_8)
    finally is.close()
  }
}
