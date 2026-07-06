package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.components.*

object CodeRenderer {
  def apply(language: String, source: String): Modifier[HtmlElement] = {
    val _ = language
    Code(
      Code.block   := true,
      Code.variant := Code.Variant.Tinted,
      Code.text    := source
    )
  }
}
