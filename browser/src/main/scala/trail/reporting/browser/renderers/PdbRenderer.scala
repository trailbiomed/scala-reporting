package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.style.*
import org.scalajs.dom
import scala.scalajs.js
import trail.reporting.schema.{DataItem, PdbColor, PdbStyle}

/** Renders a PDB structure with bio-pv (WebGL).
  *
  * bio-pv is loaded as a global (`window.pv`) by the report harness only when the
  * document actually contains a `PdbItem`. Initialization is deferred until the
  * host element has non-zero dimensions so the viewer works correctly on tabs that
  * start hidden behind Show/display:none.
  */
object PdbRenderer {

  def apply(item: DataItem.PdbItem): Modifier[HtmlElement] = {
    var viewer:  js.UndefOr[js.Dynamic]         = js.undefined
    var sizeObs: js.UndefOr[dom.ResizeObserver] = js.undefined
    var fitObs:  js.UndefOr[dom.ResizeObserver] = js.undefined

    val host = div(
      styleAttr := s"width:100%;height:min(${item.height}px, 70vh);position:relative;overflow:hidden;"
    )
    host.amend(
      onMountUnmountCallback(
        mount = { ctx =>
          val node = ctx.thisNode.ref
          val pv   = js.Dynamic.global.pv
          if (js.isUndefined(pv) || pv == null) node.textContent = "bio-pv not loaded"
          else {
            def tryInit(): Boolean = {
              val rect = node.getBoundingClientRect()
              if (rect.width <= 0 || rect.height <= 0) false
              else {
                val v = createViewer(node, item, pv)
                viewer = v
                fitObs = attachFitObserver(node, v)
                true
              }
            }

            if (!tryInit()) {
              val ro = new dom.ResizeObserver((_, obs) => if (tryInit()) obs.disconnect())
              ro.observe(node)
              sizeObs = ro
            }
          }
        },
        unmount = { _ =>
          sizeObs.foreach(_.disconnect())
          fitObs.foreach(_.disconnect())
          viewer.foreach { v =>
            try v.applyDynamic("destroy")()
            catch { case _: Throwable => () }
          }
          viewer = js.undefined
          sizeObs = js.undefined
          fitObs = js.undefined
        }
      )
    )
    host
  }

  private def createViewer(node: dom.HTMLElement, item: DataItem.PdbItem, pv: js.Dynamic): js.Dynamic = {
    val opts = js.Dynamic.literal(
      width      = "auto",
      height     = "auto",
      antialias  = true,
      quality    = "medium",
      background = item.background,
      outline    = true,
      fog        = true
    )
    val v = pv.applyDynamic("Viewer")(node, opts)

    // pv defers _initViewer until DOMContentLoaded when readyState === "loading";
    // our Scala.js bundle also runs during body parsing, so cartoon() before
    // viewerReady dereferences an undefined _gl. Register on viewerReady — pv
    // fires it immediately if it's already initialized.
    val onReady: js.Function2[js.Any, js.Any, Unit] = (_, _) => drawStructure(item, pv, v)
    val _ = v.applyDynamic("on")("viewerReady", onReady)
    v
  }

  private def drawStructure(item: DataItem.PdbItem, pv: js.Dynamic, viewer: js.Dynamic): Unit = {
    val structure  = pv.io.applyDynamic("pdb")(item.pdb)
    val colorFn    = colorFor(pv, item.color)
    val renderOpts = js.Dynamic.literal(color = colorFn)

    val method = item.style match {
      case PdbStyle.Cartoon        => "cartoon"
      case PdbStyle.Trace          => "trace"
      case PdbStyle.LineTrace      => "lineTrace"
      case PdbStyle.Sline          => "sline"
      case PdbStyle.Tube           => "tube"
      case PdbStyle.BallsAndSticks => "ballsAndSticks"
      case PdbStyle.Spheres        => "spheres"
      case PdbStyle.Points         => "points"
      case PdbStyle.Lines          => "lines"
    }
    val _ = viewer.applyDynamic(method)("structure", structure, renderOpts)
    val __ = viewer.applyDynamic("autoZoom")()
  }

  private def attachFitObserver(node: dom.HTMLElement, viewer: js.Dynamic): dom.ResizeObserver = {
    val ro = new dom.ResizeObserver((_, _) =>
      try viewer.applyDynamic("fitParent")()
      catch { case _: Throwable => () }
    )
    ro.observe(node)
    ro
  }

  private def colorFor(pv: js.Dynamic, color: PdbColor): js.Dynamic = color match {
    case PdbColor.SsSuccession  => pv.color.applyDynamic("ssSuccession")()
    case PdbColor.BySs          => pv.color.applyDynamic("bySS")()
    case PdbColor.ByChain       => pv.color.applyDynamic("byChain")()
    case PdbColor.Rainbow       => pv.color.applyDynamic("rainbow")()
    case PdbColor.Uniform       => pv.color.applyDynamic("uniform")("#4a90e2")
    case PdbColor.ByResidueProp => pv.color.applyDynamic("byResidueProp")("num")
  }
}
