package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.style.*
import org.scalajs.dom

object PlotRenderer {

  def apply(svg: String): HtmlElement = apply(svg, maxHeight = "80vh")

  /** Embed a raw SVG payload so it scales with its container.
    *
    * Fit strategy: ensure the SVG carries a `viewBox` (synthesised from any width/height
    * attributes when the source is missing one), drop intrinsic `width`/`height` so CSS wins,
    * and set `width: 100%; height: auto` bounded by `max-height`. Without the viewBox
    * fallback, a plotting library that emits pixel-sized `<svg width="800" height="600">`
    * would keep pixel-positioned content that clips instead of scales when the container
    * shrinks — which is exactly what happens with nspl/matplotlib output.
    */
  def apply(svg: String, maxHeight: String): HtmlElement = {
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
        Option(node.querySelector("svg")).foreach(el => makeResponsive(el.asInstanceOf[dom.Element], maxHeight))
      }
    )
    host
  }

  private[browser] def makeResponsive(svg: dom.Element, maxHeight: String): Unit = {
    if (!svg.hasAttribute("viewBox")) {
      val w = numericAttr(svg, "width")
      val h = numericAttr(svg, "height")
      if (w > 0 && h > 0) svg.setAttribute("viewBox", s"0 0 $w $h")
    }
    svg.removeAttribute("width")
    svg.removeAttribute("height")
    if (!svg.hasAttribute("preserveAspectRatio")) {
      svg.setAttribute("preserveAspectRatio", "xMidYMid meet")
    }
    svg.setAttribute(
      "style",
      s"width: 100%; height: auto; max-width: 100%; max-height: $maxHeight; display: block; margin: 0 auto;"
    )
  }

  private def numericAttr(svg: dom.Element, name: String): Double = {
    val raw = svg.getAttribute(name)
    if (raw == null || raw.isEmpty) 0.0
    else {
      val digits = raw.takeWhile(c => c.isDigit || c == '.' || c == '-')
      digits.toDoubleOption.getOrElse(0.0)
    }
  }
}
