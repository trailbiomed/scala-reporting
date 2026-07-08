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
