package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, Button as _, *}
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.SourceFile

object Header {

  def apply(app: App): HtmlElement =
    div(
      themed(t =>
        stack.between(spacing.lg) ++
          css.padding(spacing.lg, spacing.xxl) ++
          css.raw("border-bottom", s"1px solid ${t.border.toCss}") ++
          css.background(t.surface) ++
          css.position("sticky") ++
          css.raw("top", "0") ++
          css.zIndex(20) ++
          stack.noShrink
      ),
      div(
        stack.row(spacing.md) ++ css.alignItems("center"),
        child.maybe <-- app.docVar.signal.map(_.logo.map(logoElement)),
        span(
          themed(t =>
            css.fontSize(fontSizes.display) ++ css.fontWeight(FontWeight.SemiBold) ++ css.color(t.text)
          ),
          child.text <-- app.docVar.signal.map(_.title)
        ),
        span(
          typo.muted,
          child.text <-- app.docVar.signal.map(_.version.fold("")(v => s"v$v"))
        )
      ),
      div(
        stack.row(spacing.md) ++ css.alignItems("center"),
        span(typo.hint, child.text <-- app.docVar.signal.map(_.createdAt)),
        child.maybe <-- app.docVar.signal.map(_.source.map(downloadButton)),
        ThemePicker()
      )
    )

  private def logoElement(svg: String): HtmlElement = {
    val host = div(
      css.raw("display", "inline-flex") ++
        css.alignItems("center") ++
        css.raw("height", "32px")
    )
    host.amend(
      onMountCallback { ctx =>
        val node = ctx.thisNode.ref
        node.innerHTML = svg
        val child = node.querySelector("svg")
        if (child != null) {
          child.setAttribute("style", "height:100%;width:auto;display:block")
        }
      }
    )
    host
  }

  private def downloadButton(sf: SourceFile): HtmlElement = {
    val btn = Button(
      Button.label   := s"Source (${sf.filename})",
      Button.variant := Button.Variant.Secondary,
      Button.size    := Button.Size.Small
    )
    btn.root.amend(
      btn.clicks --> Observer[Unit](_ =>
        Downloads.fromBase64(sf.contentBase64, sf.filename, sf.mimeType)
      )
    )
    btn.root
  }
}
