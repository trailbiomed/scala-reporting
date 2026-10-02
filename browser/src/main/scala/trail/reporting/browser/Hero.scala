package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.*
import lui.style.*
import trail.reporting.schema.{DataItem, PageHero, PageTag}

object Hero {

  private val titleSize = Length.px(28)

  def apply(hero: PageHero, fallbackTitle: String): HtmlElement = {
    val headline = hero.title.filter(_.nonEmpty).getOrElse(fallbackTitle)
    div(
      themed(t =>
        stack.col(spacing.lg) ++
          css.padding(spacing.xxxl) ++
          css.borderRadius(radius.lg) ++
          css.background(t.surface) ++
          css.border(Length.px(1), BorderStyle.Solid, t.border)
      ),
      hero.image.map(imageBanner),
      hero.eyebrow.filter(_.nonEmpty).map(eyebrowLine),
      span(
        themed(t =>
          css.fontSize(titleSize) ++
            css.fontWeight(FontWeight.SemiBold) ++
            css.color(t.text) ++
            css.raw("line-height", "1.2") ++
            css.raw("letter-spacing", "-0.01em")
        ),
        headline
      ),
      hero.lead.filter(_.nonEmpty).map(leadLine),
      Option.when(hero.meta.nonEmpty)(metaRow(hero.meta))
    )
  }

  private def eyebrowLine(text: String): HtmlElement =
    span(typo.eyebrow, themed(t => css.color(t.brand)), text)

  private def leadLine(text: String): HtmlElement =
    span(
      themed(t =>
        css.fontSize(fontSizes.xxl) ++
          css.color(t.textMuted) ++
          css.raw("line-height", "1.6") ++
          css.raw("max-width", "68ch")
      ),
      text
    )

  private def imageBanner(image: DataItem.ImageItem): HtmlElement =
    div(
      css.raw("width", "100%") ++
        css.raw("overflow", "hidden") ++
        css.borderRadius(radius.md),
      img(
        src := s"data:${image.mimeType};base64,${image.base64}",
        alt := image.alt,
        css.raw("width", "100%") ++
          css.raw("height", "240px") ++
          css.raw("object-fit", "cover") ++
          css.raw("display", "block")
      )
    )

  private def metaRow(meta: Seq[PageTag]): HtmlElement =
    div(
      css.raw("display", "flex") ++
        css.raw("flex-wrap", "wrap") ++
        css.raw("column-gap", spacing.xxl.toCss) ++
        css.raw("row-gap", spacing.lg.toCss) ++
        css.raw("align-items", "flex-start"),
      meta.map(metaChip)
    )

  private def metaChip(tag: PageTag): HtmlElement =
    div(
      stack.col(Length.px(2)),
      span(typo.eyebrow, tag.name),
      span(
        themed(t =>
          css.color(t.text) ++
            css.fontWeight(FontWeight.Medium) ++
            css.fontSize(fontSizes.lg)
        ),
        tag.value
      )
    )
}
