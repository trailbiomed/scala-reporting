package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.style.*
import trail.reporting.schema.DataItem

object ImageRenderer {

  def apply(item: DataItem.ImageItem): HtmlElement = apply(item, maxHeight = "80vh")

  def apply(item: DataItem.ImageItem, maxHeight: String): HtmlElement =
    div(
      stack.col(spacing.sm) ++
        css.raw("width", "100%") ++
        css.raw("max-width", "100%") ++
        css.raw("overflow", "hidden") ++
        css.raw("text-align", "center"),
      img(
        src := s"data:${item.mimeType};base64,${item.base64}",
        alt := item.alt,
        css.raw("max-width", "100%") ++
          css.raw("max-height", maxHeight) ++
          css.raw("display", "block") ++
          css.raw("margin", "0 auto")
      )
    )
}
