package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.*
import lui.style.*
import trail.reporting.schema.{Page, PageItemMenu}

object PageView {

  def apply(page: Page, app: App): HtmlElement = {
    val heroEl: List[HtmlElement] =
      page.hero.map(h => Hero(h, page.title)).toList

    val menu: List[HtmlElement] =
      (page.itemMenu match {
        case PageItemMenu.Popover if page.items.nonEmpty =>
          Some(ItemMenuPopover(page, app))
        case _ => None
      }).toList

    val cards: List[HtmlElement] =
      page.items.zipWithIndex.map { case (item, idx) => ItemCard(item, idx, app) }.toList

    div(
      stack.col(spacing.xxl) ++ css.padding(spacing.xxl),
      heroEl ++ menu ++ cards
    )
  }
}
