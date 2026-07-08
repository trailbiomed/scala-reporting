package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, Button as _, *}
import org.scalajs.dom
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.{Item, Page}

object ItemMenuPopover {

  def apply(page: Page, app: App): HtmlElement = {
    val open    = Var(false)
    val jumpBus = new EventBus[String]

    div(
      css.raw("display", "flex") ++ css.raw("justify-content", "flex-start"),
      Popover(
        Popover.open      <--> open,
        Popover.placement := Popover.Placement.Bottom,
        Popover.trigger(triggerChip(page, open.signal)),
        Popover.body(
          menuPanel(page.items.toList, app, jumpBus.writer),
          jumpBus.events.mapTo(false) --> open.writer
        )
      )
    )
  }

  private def triggerChip(page: Page, openSig: Signal[Boolean]): HtmlElement =
    div(
      tabIndex := 0,
      openSig.styled { (t, isOpen) =>
        val (bg, fg) =
          if (isOpen) (t.brandSoft, t.brand)
          else        (t.surface, t.textMuted)
        stack.row(spacing.sm) ++
          css.padding(Length.px(4), spacing.md) ++
          css.borderRadius(radius.pill) ++
          css.background(bg) ++
          css.color(fg) ++
          css.cursor("pointer") ++
          css.fontSize(fontSizes.sm) ++
          css.raw("user-select", "none") ++
          css.transition("background-color", 120)
      },
      span("Items"),
      span(themed(t => css.color(t.textSubtle) ++ css.fontSize(fontSizes.xs)), s"${page.items.size}"),
      span(themed(t => css.color(t.textSubtle)), "▾")
    )

  private def menuPanel(items: List[Item], app: App, jumpObs: Observer[String]): HtmlElement =
    div(
      stack.col(Length.px(2)) ++
        css.raw("min-width", "220px") ++
        css.raw("max-width", "320px") ++
        css.raw("max-height", "min(60vh, 420px)") ++
        css.raw("overflow-y", "auto") ++
        css.raw("overscroll-behavior", "contain"),
      items.zipWithIndex.map { case (i, idx) => menuEntry(i, idx, app, jumpObs) }
    )

  private def menuEntry(item: Item, index: Int, app: App, jumpObs: Observer[String]): HtmlElement = {
    val hovered = Var(false)
    div(
      Signal.combine(app.activeItemVar.signal, hovered.signal).styled { case (t, (active, h)) =>
        val selected = active.contains(item.id)
        val (bg, fg) =
          if (selected) (t.brandSoft, t.brand)
          else if (h)   (t.surfaceDim, t.text)
          else          (Color.transparent, t.text)
        stack.row(spacing.sm) ++
          css.padding(Length.px(4), spacing.md) ++
          css.borderRadius(radius.sm) ++
          css.background(bg) ++
          css.color(fg) ++
          css.cursor("pointer") ++
          css.raw("user-select", "none") ++
          css.fontSize(fontSizes.md) ++
          css.fontWeight(if (selected) FontWeight.Medium else FontWeight.Regular)
      },
      onMouseEnter.mapTo(true)  --> hovered.writer,
      onMouseLeave.mapTo(false) --> hovered.writer,
      onClick.mapTo(Some(item.id)) --> app.activeItemVar.writer,
      onClick.mapTo(item.id)      --> jumpObs,
      onClick.mapTo(s"item-${item.id}") --> Observer[String] { id =>
        val node = dom.document.getElementById(id)
        if (node != null) node.scrollIntoView(true)
      },
      span(themed(t => css.color(t.textSubtle)), s"${index + 1}."),
      span(item.title)
    )
  }
}
