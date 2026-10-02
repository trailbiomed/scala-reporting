package trail.reporting.browser

import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.StringAsIsCodec
import lui.style.*

object A11y {

  val activationKeys: Set[String] = Set("Enter", " ")

  val scope: HtmlAttr[String]       = htmlAttr("scope", StringAsIsCodec)
  val ariaSort: HtmlAttr[String]    = htmlAttr("aria-sort", StringAsIsCodec)
  val ariaCurrent: HtmlAttr[String] = htmlAttr("aria-current", StringAsIsCodec)

  def activate(obs: Observer[Unit], stopPropagation: Boolean = false): Modifier[HtmlElement] =
    Modifier[HtmlElement] { el =>
      val clickUnit =
        if (stopPropagation) onClick.stopPropagation.mapToUnit else onClick.mapToUnit
      val keyUnit = {
        val filtered = onKeyDown.filter(e => activationKeys.contains(e.key))
        val scoped   = if (stopPropagation) filtered.stopPropagation else filtered
        scoped.map(e => e.preventDefault())
      }
      el.amend(clickUnit --> obs, keyUnit --> obs)
    }

  def focusRing(t: Theme, focused: Boolean): Style =
    if (focused) css.raw("box-shadow", s"0 0 0 3px ${t.brand.alpha(0.35).toCss}")
    else css.raw("box-shadow", "none")
}
