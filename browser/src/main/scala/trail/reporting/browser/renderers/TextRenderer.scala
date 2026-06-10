package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.components.*
import lui.style.*

object TextRenderer {
  def apply(content: String): Modifier[HtmlElement] =
    div(typo.body, content)
}
