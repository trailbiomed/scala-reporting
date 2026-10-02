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
        items.zipWithIndex.map { case (i, idx) => sidebarItem(i, idx, app) }.toList
      }
    )

  private def sidebarItem(item: Item, index: Int, app: App): HtmlElement = {
    val hovered = Var(false)
    val focused = Var(false)
    val activeSig = app.activeItemVar.signal.map(_.contains(item.id)).distinct
    val activate = Observer[Unit] { _ =>
      app.activeItemVar.set(Some(item.id))
      val node = dom.document.getElementById(s"item-${item.id}")
      if (node != null) node.scrollIntoView(true)
    }
    div(
      dataAttr("item-id") := item.id,
      role     := "button",
      tabIndex := 0,
      A11y.ariaCurrent <-- activeSig.map(a => if (a) "true" else null),
      Signal.combine(app.activeItemVar.signal, hovered.signal, focused.signal).styled {
        case (t, (active, h, foc)) =>
          val selected = active.contains(item.id)
          val (bg, fg) =
            if (selected) (t.brandSoft, t.brand)
            else if (h)   (t.surfaceDim, t.text)
            else          (lui.style.Color.transparent, t.textMuted)
          stack.row(spacing.sm) ++
            css.padding(Length.px(4), spacing.md) ++
            css.borderRadius(radius.sm) ++
            css.background(bg) ++
            css.color(fg) ++
            css.cursor("pointer") ++
            css.fontSize(fontSizes.lg) ++
            css.fontWeight(if (selected) FontWeight.Medium else FontWeight.Regular) ++
            A11y.focusRing(t, foc)
      },
      onMouseEnter.mapTo(true)  --> hovered.writer,
      onMouseLeave.mapTo(false) --> hovered.writer,
      onFocus.mapTo(true)  --> focused.writer,
      onBlur.mapTo(false)  --> focused.writer,
      A11y.activate(activate),
      span(themed(t => css.color(t.textSubtle)), s"${index + 1}."),
      span(item.title)
    )
  }
}
