package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.{Item, DataItem}

object ItemCard {

  def apply(item: Item, app: App): HtmlElement =
    div(
      idAttr := s"item-${item.id}",
      stack.col(spacing.lg),
      Card(
        Card.padding := spacing.xxl,
        Card.children(
          div(
            stack.col(spacing.lg),
            div(
              stack.between(spacing.md),
              span(typo.h2, item.title)
            ),
            item.data.map(d => renderData(d, app))
          )
        )
      )
    )

  private def renderData(d: DataItem, app: App): Modifier[HtmlElement] = d match {
    case DataItem.TextItem(content)        => renderers.TextRenderer(content)
    case DataItem.CodeItem(lang, source)   => renderers.CodeRenderer(lang, source)
    case DataItem.TableItem(table)         => renderers.TableRenderer(table)
    case DataItem.PlotItem(svg)            => renderers.PlotRenderer(svg)
  }
}
