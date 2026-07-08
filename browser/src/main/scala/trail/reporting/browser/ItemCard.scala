package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.*
import lui.style.*
import trail.reporting.schema.{Item, DataItem}

object ItemCard {

  def apply(item: Item, index: Int, app: App): HtmlElement =
    div(
      idAttr := s"item-${item.id}",
      themed(t =>
        stack.col(spacing.lg) ++
          css.padding(spacing.xxl) ++
          css.borderRadius(radius.md) ++
          css.background(t.surface)
      ),
      div(
        stack.row(spacing.md) ++ css.alignItems("baseline"),
        span(
          themed(t => css.color(t.textSubtle) ++ css.fontWeight(FontWeight.Regular)),
          typo.h2,
          s"${index + 1}."
        ),
        span(typo.h2, item.title)
      ),
      item.data.map(d => renderData(d, app))
    )

  private def renderData(d: DataItem, app: App): Modifier[HtmlElement] = d match {
    case DataItem.TextItem(content)        => renderers.TextRenderer(content)
    case DataItem.CodeItem(lang, source)   => renderers.CodeRenderer(lang, source)
    case DataItem.TableItem(table)         => renderers.TableRenderer(table)
    case DataItem.PlotItem(svg)            => renderers.PlotRenderer(svg)
    case pdb: DataItem.PdbItem             => renderers.PdbRenderer(pdb)
    case DataItem.CustomItem(kind, payload) =>
      app.customRenderers.get(kind) match {
        case Some(render) => render(payload)
        case None         => div(css.raw("opacity", "0.7"), s"No renderer registered for custom item kind '$kind'")
      }
  }
}
