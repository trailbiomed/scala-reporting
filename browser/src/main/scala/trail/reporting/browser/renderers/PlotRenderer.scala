package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.style.*

object PlotRenderer {

  def apply(svg: String): Modifier[HtmlElement] = {
    val host = div(
      stack.col(spacing.sm) ++
        css.raw("width", "100%") ++
        css.raw("max-width", "100%") ++
        css.raw("overflow", "hidden") ++
        css.raw("text-align", "center")
    )
    host.amend(
      onMountCallback { ctx =>
        val node = ctx.thisNode.ref
        node.innerHTML = svg
        Option(node.querySelector("svg")).foreach { svgEl =>
          svgEl.setAttribute(
            "style",
            "max-width: 100%; max-height: 80vh; width: auto; height: auto; display: block; margin: 0 auto;"
          )
          if (!svgEl.hasAttribute("preserveAspectRatio")) {
            svgEl.setAttribute("preserveAspectRatio", "xMidYMid meet")
          }
        }
      }
    )
    host
  }
}
