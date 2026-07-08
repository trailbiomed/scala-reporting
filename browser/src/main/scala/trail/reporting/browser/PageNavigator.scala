package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import org.scalajs.dom
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.{Item, Page, PageItemMenu, PageTag}

object PageNavigator {

  def apply(app: App, currentPage: Signal[Option[Page]]): HtmlElement =
    div(
      themed(t =>
        css.background(t.surface) ++
          stack.noShrink ++
          css.raw("align-self", "stretch") ++
          css.raw("box-sizing", "border-box")
      ),
      div(
        stack.col(spacing.md) ++
          css.width(Length.px(280)) ++
          css.padding(spacing.xl, spacing.lg) ++
          css.position("sticky") ++
          css.raw("top", "57px") ++
          css.raw("max-height", "calc(100vh - 57px)") ++
          css.raw("overflow-y", "auto") ++
          css.raw("box-sizing", "border-box"),
        span(typo.eyebrow, "Pages"),
        children <-- app.docVar.signal.map(_.pages.toList).map { pages =>
          pages.map(p => pageCard(p, app)).toList
        }
      )
    )

  private def pageCard(page: Page, app: App): HtmlElement = {
    val hovered   = Var(false)
    val activeSig = app.activePageVar.signal.map(_ == page.id).distinct

    val nameMod: Option[HtmlElement] =
      page.name.filter(_.nonEmpty).map(n => span(typo.eyebrow, n))
    val tagsMod: Option[HtmlElement] =
      if (page.tags.nonEmpty) Some(tagRow(page.tags)) else None

    div(
      dataAttr("page-id") := page.id,
      Signal.combine(activeSig, hovered.signal).styled { case (t, (active, h)) =>
        val bg =
          if (active) t.brandSoft
          else if (h) t.surfaceDim
          else        Color.transparent
        stack.col(spacing.sm) ++
          css.padding(spacing.md, spacing.lg) ++
          css.borderRadius(radius.md) ++
          css.background(bg) ++
          css.cursor("pointer") ++
          css.transition("background-color", 120)
      },
      onMouseEnter.mapTo(true)  --> hovered.writer,
      onMouseLeave.mapTo(false) --> hovered.writer,
      onClick.mapTo(page.id) --> app.activePageVar.writer,
      cardHeader(page, activeSig, nameMod),
      tagsMod,
      child.maybe <-- activeSig.map { active =>
        if (active && page.itemMenu == PageItemMenu.Inline && page.items.nonEmpty)
          Some(itemList(page.items.toList, app))
        else None
      }
    )
  }

  private def cardHeader(page: Page, activeSig: Signal[Boolean], nameSlot: Option[HtmlElement]): HtmlElement =
    div(
      stack.col(Length.px(2)),
      nameSlot,
      span(
        activeSig.styled { (t, active) =>
          css.fontSize(fontSizes.lg) ++
            css.fontWeight(if (active) FontWeight.SemiBold else FontWeight.Medium) ++
            css.color(if (active) t.brand else t.text)
        },
        page.title
      )
    )

  private def tagRow(tags: Seq[PageTag]): HtmlElement =
    div(
      css.raw("display", "flex") ++
        css.raw("flex-wrap", "wrap") ++
        css.raw("column-gap", spacing.md.toCss) ++
        css.raw("row-gap", Length.px(2).toCss),
      tags.map(tagChip)
    )

  private def tagChip(tag: PageTag): HtmlElement =
    span(
      themed(t =>
        stack.row(spacing.xs) ++
          css.fontSize(fontSizes.xs)
      ),
      span(themed(t => css.color(t.textSubtle)), s"${tag.name}:"),
      span(themed(t => css.color(t.text) ++ css.fontWeight(FontWeight.Medium)), tag.value)
    )

  private def itemList(items: List[Item], app: App): HtmlElement =
    div(
      stack.col(Length.px(2)),
      themed(t =>
        css.raw("border-top", s"1px dashed ${t.border.toCss}") ++
          css.raw("padding-top", spacing.sm.toCss)
      ),
      items.zipWithIndex.map { case (i, idx) => itemRow(i, idx, app) }
    )

  private def itemRow(item: Item, index: Int, app: App): HtmlElement = {
    val hovered = Var(false)
    div(
      dataAttr("item-id") := item.id,
      Signal.combine(app.activeItemVar.signal, hovered.signal).styled { case (t, (active, h)) =>
        val selected = active.contains(item.id)
        val (bg, fg) =
          if (selected) (t.brandSoft, t.brand)
          else if (h)   (t.surfaceDim, t.text)
          else          (Color.transparent, t.textMuted)
        stack.row(spacing.sm) ++
          css.padding(Length.px(3), spacing.md) ++
          css.borderRadius(radius.sm) ++
          css.background(bg) ++
          css.color(fg) ++
          css.cursor("pointer") ++
          css.fontSize(fontSizes.md) ++
          css.fontWeight(if (selected) FontWeight.Medium else FontWeight.Regular)
      },
      onMouseEnter.mapTo(true)  --> hovered.writer,
      onMouseLeave.mapTo(false) --> hovered.writer,
      onClick.stopPropagation.mapTo(Some(item.id)) --> app.activeItemVar.writer,
      onClick.stopPropagation.mapTo(s"item-${item.id}") --> Observer[String] { id =>
        val node = dom.document.getElementById(id)
        if (node != null) node.scrollIntoView(true)
      },
      span(themed(t => css.color(t.textSubtle)), s"${index + 1}."),
      span(item.title)
    )
  }
}
