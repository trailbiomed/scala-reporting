package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.*

final class App private[browser] (val root: HtmlElement) extends Component {
  private[browser] val docVar: Var[Document]              = Var(App.placeholder)
  private[browser] val activePageVar: Var[String]         = Var("")
  private[browser] val activeItemVar: Var[Option[String]] = Var(None)
}

object App extends ComponentFactory[App] {

  private val placeholder: Document =
    Document(
      title     = "Report",
      pages     = Seq.empty,
      createdAt = "",
      version   = None,
      source    = None
    )

  def apply(doc: Document): App = apply(App.document := doc)

  val document = Prop.in[Document, App](_.docVar)

  override protected def build: App = {
    val root = div()
    val el   = new App(root)

    val pagesSig: Signal[Seq[Page]] = el.docVar.signal.map(_.pages).distinct
    val currentPage: Signal[Option[Page]] = Signal.combine(pagesSig, el.activePageVar.signal).map {
      case (pages, id) => pages.find(_.id == id).orElse(pages.headOption)
    }

    val pagePanels: Signal[List[HtmlElement]] = pagesSig.map { pages =>
      pages.map(p => pagePanel(p, el)).toList
    }

    root.amend(
      themed(t =>
        stack.col(Length.zero) ++
          css.raw("min-height", "100vh") ++
          css.background(t.bg)
      ),
      Header(el),
      pageTabs(el),
      div(
        stack.row(Length.zero) ++ css.alignItems("flex-start") ++ stack.grow ++ css.raw("min-height", "0"),
        Sidebar(el, currentPage),
        div(
          stack.fill ++ css.raw("min-width", "0"),
          children <-- pagePanels
        )
      ),
      pagesSig.map(_.headOption.map(_.id).getOrElse("")).distinct --> el.activePageVar.writer,
      currentPage.map(_.flatMap(_.items.headOption.map(_.id))).distinct --> el.activeItemVar.writer
    )

    el
  }

  private def pagePanel(page: Page, el: App): HtmlElement =
    Show(
      Show.visible <-- el.activePageVar.signal.map(_ == page.id),
      Show.content(PageView(page, el))
    ).root
}
