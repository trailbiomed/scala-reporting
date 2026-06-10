package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.style.*

object PlotRenderer {

  def apply(svg: String): Modifier[HtmlElement] = {
    val host = div(stack.col(spacing.sm))
    host.amend(
      onMountCallback { ctx =>
        ctx.thisNode.ref.innerHTML = svg
      }
    )
    host
  }
}
