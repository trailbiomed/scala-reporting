package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.components.*
import lui.style.*

object CodeRenderer {
  def apply(language: String, source: String): Modifier[HtmlElement] = {
    val _ = language
    div(
      stack.col(spacing.xs),
      Code(
        Code.block := true,
        Code.text  := source
      )
    )
  }
}
