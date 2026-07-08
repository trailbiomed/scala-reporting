package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.components.*
import lui.style.*

object TextRenderer {

  def apply(content: String): Modifier[HtmlElement] =
    div(
      stack.col(spacing.md),
      Markdown.parse(content).map(renderBlock)
    )

  private def renderBlock(b: Markdown.Block): Modifier[HtmlElement] = b match {
    case Markdown.Block.Heading(level, inlines) =>
      Heading(level = math.min(level + 2, 6))(inlines.map(renderInline))
    case Markdown.Block.Paragraph(inlines) =>
      Text.body(inlines.map(renderInline))
    case Markdown.Block.Bullets(items) =>
      Listing()(items.map(is => Listing.item(Text.body(is.map(renderInline))))*)
    case Markdown.Block.CodeBlock(_, src) =>
      Code(Code.block := true, Code.variant := Code.Variant.Tinted, Code.text := src)
  }

  private def renderInline(i: Markdown.Inline): Modifier[HtmlElement] = i match {
    case Markdown.Inline.Plain(t)          => span(t)
    case Markdown.Inline.Bold(t)           => strong(t)
    case Markdown.Inline.Italic(t)         => em(t)
    case Markdown.Inline.CodeSpan(t)       => Code(Code.text := t)
    case Markdown.Inline.Link(label, href) =>
      Link(Link.href := href, Link.external := true, Link.children(label))
  }
}
