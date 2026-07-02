package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.*
import lui.style.*

object Footer {

  def apply(app: App): HtmlElement =
    div(
      themed(t =>
        css.padding(spacing.md, spacing.xxl) ++
          css.raw("border-top", s"1px solid ${t.border.toCss}") ++
          css.background(t.surface) ++
          css.color(t.textMuted) ++
          css.fontSize(fontSizes.sm) ++
          css.position("sticky") ++
          css.raw("bottom", "0") ++
          css.zIndex(10) ++
          stack.noShrink
      ),
      child.text <-- app.docVar.signal.map(_.footnote.getOrElse(""))
    )
}
