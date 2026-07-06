package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import org.scalajs.dom
import lui.*
import lui.style.*
import trail.reporting.schema.{Item, Page}

object Sidebar {

  def apply(app: App, currentPage: Signal[Option[Page]]): HtmlElement =
    div(
      themed(t =>
        stack.col(spacing.xs) ++
          css.width(Length.px(240)) ++
          stack.noShrink ++
          css.padding(spacing.xl, spacing.lg) ++
          css.raw("border-right", s"1px solid ${t.border.toCss}") ++
          css.position("sticky") ++
          css.raw("top", "105px") ++
          css.raw("max-height", "calc(100vh - 105px)") ++
          css.raw("overflow-y", "auto") ++
          css.raw("box-sizing", "border-box")
      ),
      span(typo.eyebrow, "Items"),
      children <-- currentPage.map(_.fold(List.empty[Item])(_.items.toList)).map { items =>
        items.map(i => sidebarItem(i, app)).toList
      }
    )

  private def sidebarItem(item: Item, app: App): HtmlElement = {
    val hovered = Var(false)
    div(
      dataAttr("item-id") := item.id,
      Signal.combine(app.activeItemVar.signal, hovered.signal).styled { case (t, (active, h)) =>
        val selected = active.contains(item.id)
        val (bg, fg) =
          if (selected) (t.brandSoft, t.brand)
          else if (h)   (t.surfaceDim, t.text)
          else          (lui.style.Color.transparent, t.textMuted)
        css.padding(Length.px(4), spacing.md) ++
          css.borderRadius(radius.sm) ++
          css.background(bg) ++
          css.color(fg) ++
          css.cursor("pointer") ++
          css.fontSize(fontSizes.lg) ++
          css.fontWeight(if (selected) FontWeight.Medium else FontWeight.Regular)
      },
      onMouseEnter.mapTo(true)  --> hovered.writer,
      onMouseLeave.mapTo(false) --> hovered.writer,
      onClick.mapTo(Some(item.id)) --> app.activeItemVar.writer,
      onClick.mapTo(s"item-${item.id}") --> Observer[String] { id =>
        val node = dom.document.getElementById(id)
        if (node != null) node.scrollIntoView(true)
      },
      item.title
    )
  }
}
