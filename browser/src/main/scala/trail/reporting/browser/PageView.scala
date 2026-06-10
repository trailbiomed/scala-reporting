package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.*
import lui.style.*
import trail.reporting.schema.Page

object PageView {

  def apply(page: Page, app: App): HtmlElement =
    div(
      stack.col(spacing.xxl) ++ css.padding(spacing.xxl),
      page.items.map(item => ItemCard(item, app))
    )
}
