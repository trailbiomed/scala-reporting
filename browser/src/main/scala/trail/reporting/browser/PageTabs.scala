package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.components.*
import lui.style.*

def pageTabs(app: App): HtmlElement =
  div(
    themed(t =>
      css.padding(Length.zero, spacing.xxl) ++
        css.background(t.surface) ++
        css.position("sticky") ++
        css.raw("top", "57px") ++
        css.zIndex(15) ++
        stack.noShrink
    ),
    Tabs(
      Tabs.variant := Tabs.Variant.Underlined,
      Tabs.tabs   <-- app.docVar.signal.map(_.pages.map(p => (p.id, p.title))),
      Tabs.active <--> app.activePageVar
    )
  )
