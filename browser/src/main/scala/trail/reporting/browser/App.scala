package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, *}
import org.scalajs.dom
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.*

final class App private[browser] (val root: HtmlElement) extends Component {
  private[browser] val docVar: Var[Document]              = Var(App.placeholder)
  private[browser] val activePageVar: Var[String]         = Var("")
  private[browser] val activeItemVar: Var[Option[String]] = Var(None)

  private[browser] val slideshowOpenVar: Var[Boolean]     = Var(false)

  private[browser] val slideCursorVar: Var[(Int, Int)]    = Var((0, -1))

  private[browser] val slideHelpVar: Var[Boolean]         = Var(false)

  private[browser] val openSlideshowBus: EventBus[Unit]   = new EventBus[Unit]
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

    val hasFootnote: Signal[Boolean] =
      el.docVar.signal.map(_.footnote.exists(_.nonEmpty)).distinct

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
      child.maybe <-- hasFootnote.map(if (_) Some(Footer(el)) else None),
      child.maybe <-- el.slideshowOpenVar.signal.map { open =>
        if (open) Some(Slideshow(el)) else None
      },
      documentEvents(_.onKeyDown)
        .compose(_.withCurrentValueOf(el.slideshowOpenVar.signal))
        .collect {
          case (ev, false) if isSlideshowShortcut(ev) =>
            ev.preventDefault()
            ()
        } --> el.openSlideshowBus.writer,
      el.openSlideshowBus.events
        .compose(_.withCurrentValueOf(el.docVar.signal, el.activePageVar.signal, el.activeItemVar.signal))
        .map { case (doc, pageId, itemId) =>
          val pageIdx = math.max(0, doc.pages.indexWhere(_.id == pageId))
          val itemIdx = doc.pages.lift(pageIdx) match {
            case Some(page) => itemId.fold(-1)(id => page.items.indexWhere(_.id == id))
            case None       => -1
          }
          (pageIdx, itemIdx)
        } --> el.slideCursorVar.writer,
      el.openSlideshowBus.events.mapTo(false) --> el.slideHelpVar.writer,
      el.openSlideshowBus.events.mapTo(true)  --> el.slideshowOpenVar.writer,
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

  private def isSlideshowShortcut(ev: dom.KeyboardEvent): Boolean = {
    if (ev.ctrlKey || ev.metaKey || ev.altKey) false
    else if (ev.defaultPrevented) false
    else {
      val target = ev.target.asInstanceOf[dom.Element]
      val isInput = target != null && (target.tagName match {
        case "INPUT" | "TEXTAREA" | "SELECT" => true
        case _ =>
          val ce = target.getAttribute("contenteditable")
          ce != null && ce != "false"
      })
      !isInput && (ev.key == "f" || ev.key == "F")
    }
  }

}
