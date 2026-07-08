package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, Button as _, table as tableTag, *}
import org.scalajs.dom
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.{Column, DataItem, IntegerColumn, Item, NumberColumn, Page, PageTag, TableSpec}

object Slideshow {

  private final case class Cursor(pageIdx: Int, itemIdx: Int) {
    def isOverview: Boolean = itemIdx < 0
  }

  private object Cursor {
    def from(t: (Int, Int)): Cursor = Cursor(t._1, t._2)
    def toTuple(c: Cursor): (Int, Int) = (c.pageIdx, c.itemIdx)
  }

  private enum Axis { case Page, Item }

  def apply(app: App): HtmlElement = {
    val pagesSig: Signal[Vector[Page]] = app.docVar.signal.map(_.pages.toVector).distinct

    val cursorSig: Signal[Cursor] =
      Signal.combine(app.slideCursorVar.signal, pagesSig).map { case (raw, pages) =>
        clamp(Cursor.from(raw), pages)
      }

    val currentPageSig: Signal[Option[Page]] =
      Signal.combine(pagesSig, cursorSig).map { case (ps, c) => ps.lift(c.pageIdx) }

    val currentItemSig: Signal[Option[Item]] =
      Signal.combine(currentPageSig, cursorSig).map { case (pg, c) =>
        pg.flatMap(p => if (c.itemIdx >= 0) p.items.lift(c.itemIdx) else None)
      }

    val slideCountSig: Signal[(Int, Int)] =
      Signal.combine(pagesSig, cursorSig).map { case (ps, c) => (linearIndexOf(c, ps), linearTotal(ps)) }

    val closeBus:      EventBus[Unit]        = new EventBus[Unit]
    val stepBus:       EventBus[(Axis, Int)] = new EventBus[(Axis, Int)]
    val cursorBus:     EventBus[Cursor]      = new EventBus[Cursor]
    val helpToggleBus: EventBus[Unit]        = new EventBus[Unit]

    val closeObs:      Observer[Unit]        = closeBus.writer
    val goObs:         Observer[Cursor]      = cursorBus.writer
    val toggleHelpObs: Observer[Unit]        = helpToggleBus.writer
    def stepFor(axis: Axis, delta: Int): Observer[Unit] =
      stepBus.writer.contramap[Unit](_ => (axis, delta))
    val firstSlideObs: Observer[Unit] =
      cursorBus.writer.contramap[Unit](_ => Cursor(0, -1))

    val closeSplit: EventStream[Boolean] =
      closeBus.events.compose(_.withCurrentValueOf(app.slideHelpVar.signal))

    val closeHelpStream: EventStream[Boolean] = closeSplit.collect { case true => false }
    val exitStream:      EventStream[Unit]    = closeSplit.collect { case false => () }

    val exitNav: EventStream[(String, Option[String])] =
      exitStream.compose(_.withCurrentValueOf(app.slideCursorVar.signal, app.docVar.signal))
        .map { case ((pageIdx, itemIdx), doc) =>
          doc.pages.lift(pageIdx).map { page =>
            val itemId =
              if (itemIdx >= 0) page.items.lift(itemIdx).map(_.id)
              else page.items.headOption.map(_.id)
            (page.id, itemId)
          }
        }
        .collect { case Some(nav) => nav }

    val stepStream: EventStream[Cursor] =
      stepBus.events
        .compose(_.withCurrentValueOf(app.slideCursorVar.signal, app.docVar.signal))
        .map { case (axis, delta, cursorRaw, doc) =>
          val pages  = doc.pages.toVector
          val cursor = clamp(Cursor.from(cursorRaw), pages)
          axis match {
            case Axis.Page => stepPage(cursor, pages, delta)
            case Axis.Item => stepItem(cursor, pages, delta)
          }
        }
        .collect { case Some(c) => c }

    val helpToggleStream: EventStream[Boolean] =
      helpToggleBus.events
        .compose(_.withCurrentValueOf(app.slideHelpVar.signal))
        .map(!_)

    val keydownStream = documentEvents(_.onKeyDown)

    FullscreenOverlay(
      FullscreenOverlay.open  := true,
      FullscreenOverlay.close --> closeObs,
      FullscreenOverlay.body(
        slideBody(cursorSig, currentPageSig, currentItemSig, goObs, app.customRenderers),
        progressFooter(
          pagesSig,
          cursorSig,
          currentPageSig,
          currentItemSig,
          slideCountSig,
          prevPageObs = stepFor(Axis.Page, -1),
          nextPageObs = stepFor(Axis.Page, +1),
          prevItemObs = stepFor(Axis.Item, -1),
          nextItemObs = stepFor(Axis.Item, +1),
          firstSlide  = firstSlideObs,
          goObs       = goObs,
          closeObs    = closeObs,
          toggleHelp  = toggleHelpObs
        ),
        child.maybe <-- app.slideHelpVar.signal.map { open =>
          if (open) Some(helpPanel(toggleHelpObs)) else None
        },
        keydownStream.compose(_.withCurrentValueOf(cursorSig, pagesSig, app.slideHelpVar.signal))
          --> Observer[(dom.KeyboardEvent, Cursor, Vector[Page], Boolean)] {
            case (ev, cursor, pages, helpOpen) =>
              handleKey(ev, cursor, pages, helpOpen, closeObs, goObs, toggleHelpObs)
          },
        stepStream                             --> cursorBus.writer,
        cursorBus.events.map(Cursor.toTuple)   --> app.slideCursorVar.writer,
        helpToggleStream                       --> app.slideHelpVar.writer,
        closeHelpStream                        --> app.slideHelpVar.writer,
        exitNav.map(_._1)                      --> app.activePageVar.writer,
        exitNav.map(_._2)                      --> app.activeItemVar.writer,
        exitStream.mapTo(false)                --> app.slideshowOpenVar.writer
      )
    ).root
  }

  private def slideBody(
      cursorSig: Signal[Cursor],
      pageSig: Signal[Option[Page]],
      itemSig: Signal[Option[Item]],
      goObs: Observer[Cursor],
      customRenderers: Map[String, String => HtmlElement]
  ): HtmlElement =
    div(
      stack.grow ++ css.raw("min-height", "0") ++ css.raw("overflow", "auto"),
      div(
        css.padding(spacing.xxxl) ++
          css.raw("max-width", "min(2560px, 96vw)") ++
          css.raw("width", "100%") ++
          css.raw("box-sizing", "border-box") ++
          css.margin(Length.zero, Length.auto),
        child <-- Signal.combine(cursorSig, pageSig, itemSig).map {
          case (c, Some(page), _) if c.isOverview => overviewSlide(page, c.pageIdx, goObs)
          case (_, Some(_), Some(item))           => itemSlide(item, customRenderers)
          case _                                  => emptySlide()
        }
      )
    )

  private def overviewSlide(page: Page, pageIdx: Int, goObs: Observer[Cursor]): HtmlElement =
    div(
      stack.col(spacing.xxl),
      div(
        stack.col(spacing.sm),
        span(
          themed(t =>
            css.raw("font-size", "1.25rem") ++
              css.raw("letter-spacing", "0.08em") ++
              css.raw("text-transform", "uppercase") ++
              css.color(t.textMuted)
          ),
          page.name.filter(_.nonEmpty).getOrElse("Overview")
        ),
        span(
          themed(t =>
            css.raw("font-size", "5rem") ++
              css.raw("line-height", "1.05") ++
              css.fontWeight(FontWeight.SemiBold) ++
              css.color(t.text)
          ),
          page.title
        )
      ),
      Option.when(page.tags.nonEmpty)(slideTagRow(page.tags)),
      page.description.filter(_.nonEmpty).map { desc =>
        div(
          themed(t =>
            css.color(t.textMuted) ++
              css.raw("font-size", "1.875rem") ++
              css.raw("line-height", "1.4") ++
              css.raw("max-width", "60ch")
          ),
          desc
        )
      },
      if (page.items.isEmpty)
        div(
          themed(t => css.color(t.textMuted) ++ css.raw("font-size", "1.5rem")),
          "This page contains no items."
        )
      else
        div(
          css.raw("display", "grid") ++
            css.gridTemplateColumns("repeat(auto-fill, minmax(300px, 1fr))") ++
            css.gap(spacing.lg),
          page.items.zipWithIndex.map { case (item, itemIdx) =>
            overviewTile(item, pageIdx, itemIdx, goObs)
          }.toList
        )
    )

  private def overviewTile(item: Item, pageIdx: Int, itemIdx: Int, goObs: Observer[Cursor]): HtmlElement = {
    val hovered = Var(false)
    div(
      hovered.signal.styled { (t, h) =>
        val bg = if (h) t.brandSoft else t.surface
        stack.col(spacing.sm) ++
          css.padding(spacing.lg, spacing.xl) ++
          css.borderRadius(radius.md) ++
          css.background(bg) ++
          css.cursor("pointer") ++
          css.raw("user-select", "none") ++
          css.transition("background-color", 120)
      },
      onMouseEnter.mapTo(true)  --> hovered.writer,
      onMouseLeave.mapTo(false) --> hovered.writer,
      onClick.mapTo(Cursor(pageIdx, itemIdx)) --> goObs,
      span(
        themed(t => css.color(t.textMuted) ++ css.raw("font-size", "1rem")),
        s"${itemIdx + 1}"
      ),
      span(
        themed(t =>
          css.color(t.text) ++
            css.fontWeight(FontWeight.Medium) ++
            css.raw("font-size", "1.5rem") ++
            css.raw("line-height", "1.25")
        ),
        item.title
      )
    )
  }

  private def slideTagRow(tags: Seq[PageTag]): HtmlElement =
    div(
      css.raw("display", "flex") ++
        css.raw("flex-wrap", "wrap") ++
        css.raw("column-gap", spacing.xl.toCss) ++
        css.raw("row-gap", spacing.sm.toCss),
      tags.map(slideTagChip).toList
    )

  private def slideTagChip(tag: PageTag): HtmlElement =
    span(
      themed(t =>
        stack.row(spacing.sm) ++
          css.raw("font-size", "1.1rem") ++
          css.color(t.textMuted)
      ),
      span(themed(t => css.color(t.textSubtle)), s"${tag.name}:"),
      span(themed(t => css.color(t.text) ++ css.fontWeight(FontWeight.Medium)), tag.value)
    )

  private def itemSlide(item: Item, customRenderers: Map[String, String => HtmlElement]): HtmlElement =
    div(
      stack.col(spacing.xxl) ++ css.raw("font-size", "1.25rem"),
      span(
        themed(t =>
          css.raw("font-size", "3.75rem") ++
            css.raw("line-height", "1.1") ++
            css.fontWeight(FontWeight.SemiBold) ++
            css.color(t.text)
        ),
        item.title
      ),
      item.data.map(d => slideData(d, customRenderers))
    )

  private def slideData(d: DataItem, customRenderers: Map[String, String => HtmlElement]): Modifier[HtmlElement] = d match {
    case DataItem.TextItem(content) => slideText(content)
    case DataItem.CodeItem(lang, source) =>
      renderers.CodeRenderer(lang, source)
    case DataItem.TableItem(table) =>
      div(
        css.raw("max-height", "70vh") ++ css.raw("overflow", "auto"),
        slideTable(table)
      )
    case DataItem.PlotItem(svg)             => slidePlot(svg)
    case pdb: DataItem.PdbItem              => renderers.PdbRenderer(pdb)
    case DataItem.CustomItem(kind, payload) =>
      customRenderers.get(kind) match {
        case Some(render) => render(payload)
        case None         => div(css.raw("opacity", "0.7"), s"No renderer registered for custom item kind '$kind'")
      }
  }
  
  private def slideText(content: String): HtmlElement =
    div(
      themed(t =>
        css.color(t.text) ++
          css.raw("font-size", "1.875rem") ++
          css.raw("line-height", "1.5")
      ),
      stack.col(spacing.md),
      renderers.Markdown.parse(content).map(slideBlock)
    )

  private def slideBlock(b: renderers.Markdown.Block): HtmlElement = b match {
    case renderers.Markdown.Block.Heading(level, inlines) =>
      val size = level match {
        case 1 => "2.5rem"
        case 2 => "2.25rem"
        case _ => "2rem"
      }
      div(
        themed(t =>
          css.color(t.text) ++
            css.raw("font-size", size) ++
            css.raw("line-height", "1.2") ++
            css.fontWeight(FontWeight.SemiBold)
        ),
        inlines.map(slideInline)
      )
    case renderers.Markdown.Block.Paragraph(inlines) =>
      div(inlines.map(slideInline))
    case renderers.Markdown.Block.Bullets(items) =>
      ul(
        css.raw("padding-left", "1.5em") ++ css.raw("margin", "0"),
        items.map(is => li(is.map(slideInline)))
      )
    case renderers.Markdown.Block.CodeBlock(_, src) =>
      pre(
        themed(t =>
          css.background(t.surface) ++
            css.color(t.text) ++
            css.padding(spacing.lg) ++
            css.borderRadius(radius.md) ++
            css.raw("font-family", "ui-monospace,SFMono-Regular,monospace") ++
            css.raw("font-size", "1.5rem") ++
            css.raw("line-height", "1.4") ++
            css.raw("overflow", "auto") ++
            css.raw("margin", "0")
        ),
        src
      )
  }

  private def slideInline(i: renderers.Markdown.Inline): HtmlElement = i match {
    case renderers.Markdown.Inline.Plain(t)    => span(t)
    case renderers.Markdown.Inline.Bold(t)     => strong(t)
    case renderers.Markdown.Inline.Italic(t)   => em(t)
    case renderers.Markdown.Inline.CodeSpan(t) =>
      code(
        themed(theme =>
          css.background(theme.surface) ++
            css.raw("padding", "0.05em 0.35em") ++
            css.raw("border-radius", "0.3em") ++
            css.raw("font-family", "ui-monospace,SFMono-Regular,monospace") ++
            css.raw("font-size", "0.9em")
        ),
        t
      )
    case renderers.Markdown.Inline.Link(label, url) =>
      a(
        href   := url,
        target := "_blank",
        rel    := "noopener noreferrer",
        themed(t =>
          css.color(t.text) ++
            css.raw("text-decoration", "underline")
        ),
        label
      )
  }

  private def slidePlot(svg: String): HtmlElement = {
    val host = div(
      css.raw("max-height", "70vh") ++
        css.raw("overflow", "hidden") ++
        css.raw("text-align", "center")
    )
    host.amend(
      onMountCallback { ctx =>
        val node = ctx.thisNode.ref
        node.innerHTML = svg
        Option(node.querySelector("svg")).foreach { svgEl =>
          svgEl.setAttribute(
            "style",
            "max-width: 100%; max-height: 70vh; width: auto; height: auto; display: block; margin: 0 auto;"
          )
          if (!svgEl.hasAttribute("preserveAspectRatio")) {
            svgEl.setAttribute("preserveAspectRatio", "xMidYMid meet")
          }
        }
      }
    )
    host
  }

  private def slideTable(spec: TableSpec): HtmlElement =
    tableTag(
      themed(t =>
        css.width(Length.pct(100)) ++
          css.raw("border-collapse", "collapse") ++
          css.raw("font-size", slideTableFontSize(spec.rowCount, spec.columns.length)) ++
          css.color(t.text)
      ),
      thead(
        tr(
          spec.columns.map(col => slideHeadCell(col)).toList
        )
      ),
      tbody(
        (0 until spec.rowCount).map(i => slideBodyRow(spec, i)).toList
      )
    )

  /** Slide tables should fill available space. Pick a font-size that grows when there
    * are few rows or columns and shrinks back to the default when there are many.
    * Two independent tiers (rows, cols); the more constraining one wins so text
    * neither overflows horizontally nor makes the table taller than the slide. */
  private def slideTableFontSize(rows: Int, cols: Int): String = {
    def byRows(r: Int): Double = r match {
      case n if n <= 3  => 3.0
      case n if n <= 6  => 2.25
      case n if n <= 10 => 1.75
      case n if n <= 20 => 1.375
      case _            => 1.125
    }
    def byCols(c: Int): Double = c match {
      case n if n <= 2 => 3.0
      case n if n <= 4 => 2.25
      case n if n <= 6 => 1.75
      case n if n <= 8 => 1.375
      case _           => 1.125
    }
    f"${math.min(byRows(rows), byCols(cols))}%.3frem"
  }

  private def slideHeadCell(col: Column): HtmlElement =
    th(
      themed(t =>
        css.padding(spacing.sm, spacing.lg) ++
          css.raw("border-bottom", s"1px solid ${t.border.toCss}") ++
          css.textAlign(TextAlign.Left) ++
          css.background(t.surface) ++
          css.position("sticky") ++
          css.raw("top", "0") ++
          css.color(t.text) ++
          css.fontWeight(FontWeight.SemiBold) ++
          (if (col.isNumeric) css.textAlign(TextAlign.Right) else Style.empty)
      ),
      col.label
    )

  private def slideBodyRow(spec: TableSpec, i: Int): HtmlElement =
    tr(
      spec.columns.map(col => slideBodyCell(col, i)).toList
    )

  private def slideBodyCell(col: Column, i: Int): HtmlElement =
    td(
      rawTooltip(col, i).map(v => title := v),
      themed(t =>
        css.padding(spacing.sm, spacing.lg) ++
          css.raw("border-bottom", s"1px solid ${t.border.toCss}") ++
          css.color(if (col.isNullAt(i)) t.textSubtle else t.text) ++
          (if (col.isNumeric)
             css.textAlign(TextAlign.Right) ++ css.raw("font-variant-numeric", "tabular-nums")
           else Style.empty)
      ),
      col.displayAt(i)
    )

  private def rawTooltip(col: Column, i: Int): Option[String] = col match {
    case n: NumberColumn  if !n.isNullAt(i) => Some(n.values(i).toString)
    case n: IntegerColumn if !n.isNullAt(i) => Some(n.values(i).toString)
    case _                                  => None
  }

  private def emptySlide(): HtmlElement =
    div(typo.muted, "No slides available.")

  private def progressFooter(
      pagesSig: Signal[Vector[Page]],
      cursorSig: Signal[Cursor],
      pageSig: Signal[Option[Page]],
      itemSig: Signal[Option[Item]],
      slideCountSig: Signal[(Int, Int)],
      prevPageObs: Observer[Unit],
      nextPageObs: Observer[Unit],
      prevItemObs: Observer[Unit],
      nextItemObs: Observer[Unit],
      firstSlide:  Observer[Unit],
      goObs:       Observer[Cursor],
      closeObs:    Observer[Unit],
      toggleHelp:  Observer[Unit]
  ): HtmlElement = {
    val breadcrumbSig: Signal[String] =
      Signal.combine(pageSig, itemSig).map {
        case (Some(p), Some(i)) => s"${p.title} › ${i.title}"
        case (Some(p), None)    => s"${p.title} › Overview"
        case _                  => ""
      }

    div(
      themed(t =>
        stack.between(spacing.md) ++
          css.padding(spacing.sm, spacing.xxl) ++
          css.raw("border-top", s"1px solid ${t.border.toCss}") ++
          css.background(t.surface) ++
          stack.noShrink
      ),
      div(
        stack.row(spacing.md) ++ css.alignItems("center") ++ stack.fill,
        breadcrumbTrigger(breadcrumbSig, pagesSig, cursorSig, goObs)
      ),
      div(
        stack.row(spacing.lg) ++ css.alignItems("center") ++ stack.noShrink,
        div(
          stack.row(spacing.xs) ++ css.alignItems("center"),
          navButton("⏮", "First slide",   firstSlide),
          navButton("←",  "Previous page", prevPageObs),
          navButton("↑",  "Previous item", prevItemObs),
          navButton("↓",  "Next item",     nextItemObs),
          navButton("→",  "Next page",     nextPageObs)
        ),
        div(
          stack.row(spacing.sm) ++ css.alignItems("center"),
          span(typo.hint, child.text <-- slideCountSig.map { case (i, n) => s"$i / $n" }),
          navButton("?", "Shortcuts", toggleHelp),
          navButton("✕", "Exit slideshow", closeObs)
        )
      )
    )
  }

  private def breadcrumbTrigger(
      breadcrumbSig: Signal[String],
      pagesSig: Signal[Vector[Page]],
      cursorSig: Signal[Cursor],
      goObs: Observer[Cursor]
  ): HtmlElement = {
    val open     = Var(false)
    val jumpBus  = new EventBus[Cursor]
    Popover(
      Popover.open      <--> open,
      Popover.placement := Popover.Placement.Top,
      Popover.trigger(
        div(
          open.signal.styled { (t, isOpen) =>
            css.color(if (isOpen) t.brand else t.textMuted) ++
              css.raw("font-size", "0.95rem") ++
              css.cursor("pointer") ++
              css.raw("white-space", "nowrap") ++
              css.raw("overflow", "hidden") ++
              css.raw("text-overflow", "ellipsis") ++
              css.raw("min-width", "0") ++
              css.raw("max-width", "40vw") ++
              css.raw("user-select", "none")
          },
          child.text <-- breadcrumbSig
        )
      ),
      Popover.body(
        tocPanel(pagesSig, cursorSig, jumpBus.writer),
        jumpBus.events              --> goObs,
        jumpBus.events.mapTo(false) --> open.writer
      )
    ).root
  }

  private def tocPanel(
      pagesSig: Signal[Vector[Page]],
      cursorSig: Signal[Cursor],
      goObs: Observer[Cursor]
  ): HtmlElement =
    div(
      stack.col(spacing.md) ++
        css.raw("min-width", "240px") ++
        css.raw("max-width", "360px") ++
        css.raw("max-height", "min(60vh, 480px)") ++
        css.raw("overflow-y", "auto") ++
        css.raw("overscroll-behavior", "contain"),
      children <-- Signal.combine(pagesSig, cursorSig).map { case (pages, cursor) =>
        pages.zipWithIndex.map { case (page, pageIdx) =>
          tocPage(page, pageIdx, cursor, goObs)
        }.toList
      }
    )

  private def tocPage(page: Page, pageIdx: Int, cursor: Cursor, goObs: Observer[Cursor]): HtmlElement =
    div(
      stack.col(Length.px(2)),
      tocEntry(
        label      = page.title,
        keyGlyph   = "▸",
        active     = cursor.pageIdx == pageIdx && cursor.isOverview,
        emphasized = true,
        goObs      = goObs,
        cursor     = Cursor(pageIdx, -1)
      ),
      page.items.zipWithIndex.map { case (item, itemIdx) =>
        tocEntry(
          label      = item.title,
          keyGlyph   = "•",
          active     = cursor.pageIdx == pageIdx && cursor.itemIdx == itemIdx,
          emphasized = false,
          goObs      = goObs,
          cursor     = Cursor(pageIdx, itemIdx)
        )
      }.toList
    )

  private def tocEntry(
      label: String,
      keyGlyph: String,
      active: Boolean,
      emphasized: Boolean,
      goObs: Observer[Cursor],
      cursor: Cursor
  ): HtmlElement = {
    val hovered = Var(false)
    div(
      hovered.signal.styled { (t, h) =>
        val (bg, fg) =
          if (active) (t.brandSoft, t.brand)
          else if (h) (t.surfaceDim, t.text)
          else        (Color.transparent, t.text)
        stack.row(spacing.sm) ++
          css.padding(Length.px(4), spacing.md) ++
          css.borderRadius(radius.sm) ++
          css.background(bg) ++
          css.color(fg) ++
          css.cursor("pointer") ++
          css.raw("user-select", "none") ++
          css.fontSize(fontSizes.lg) ++
          css.fontWeight(if (active || emphasized) FontWeight.Medium else FontWeight.Regular) ++
          (if (!emphasized) css.raw("padding-left", "24px") else Style.empty)
      },
      onMouseEnter.mapTo(true)  --> hovered.writer,
      onMouseLeave.mapTo(false) --> hovered.writer,
      onClick.mapTo(cursor) --> goObs,
      span(themed(t => css.color(t.textSubtle)), keyGlyph),
      span(label)
    )
  }

  private def navButton(glyph: String, label: String, obs: Observer[Unit]): HtmlElement =
    IconButton(
      IconButton.icon      := glyph,
      IconButton.ariaLabel := label,
      IconButton.size      := IconButton.Size.Small,
      IconButton.variant   := IconButton.Variant.Ghost,
      IconButton.click     --> obs
    ).root

  private def helpPanel(toggleHelpObs: Observer[Unit]): HtmlElement = {
    val closeBtn = Button(
      Button.label   := "Close",
      Button.variant := Button.Variant.Ghost,
      Button.size    := Button.Size.Small
    )
    closeBtn.root.amend(closeBtn.clicks --> toggleHelpObs)
    div(
      themed(t =>
        css.position("fixed") ++
          css.raw("right", "16px") ++
          css.raw("bottom", "56px") ++
          css.raw("max-width", "360px") ++
          css.raw("max-height", "calc(100vh - 96px)") ++
          css.raw("overflow", "auto") ++
          css.zIndex(150) ++
          css.background(t.surface) ++
          css.border(Length.px(1), BorderStyle.Solid, t.border) ++
          css.borderRadius(radius.md) ++
          css.boxShadow(s"0 12px 32px ${t.backdrop.toCss}") ++
          css.padding(spacing.md, spacing.lg)
      ),
      div(
        stack.col(spacing.sm),
        div(
          stack.between(spacing.md) ++ css.alignItems("center"),
          span(
            themed(t => css.fontSize(fontSizes.md) ++ css.fontWeight(FontWeight.SemiBold) ++ css.color(t.text)),
            "Keyboard shortcuts"
          ),
          closeBtn.root
        ),
        shortcutTable
      )
    )
  }

  private val shortcuts: Seq[KbdList.Entry] = Seq(
    KbdList.Entry(Seq("→", "PageDown"), "Next page (tab axis)"),
    KbdList.Entry(Seq("←", "PageUp"),   "Previous page"),
    KbdList.Entry(Seq("↓", "Space"),    "Next item on this page"),
    KbdList.Entry(Seq("↑"),             "Previous item on this page"),
    KbdList.Entry(Seq("O"),             "Jump to page overview"),
    KbdList.Entry(Seq("Home"),          "First slide"),
    KbdList.Entry(Seq("End"),           "Last slide"),
    KbdList.Entry(Seq("Enter"),         "From overview → first item"),
    KbdList.Entry(Seq("?", "H"),        "Toggle this help"),
    KbdList.Entry(Seq("Esc"),           "Close help / exit slideshow")
  )

  private def shortcutTable: HtmlElement =
    KbdList(KbdList.entries := shortcuts).root

  private def handleKey(
      ev: dom.KeyboardEvent,
      cursor: Cursor,
      pages: Vector[Page],
      helpOpen: Boolean,
      closeObs: Observer[Unit],
      goObs: Observer[Cursor],
      toggleHelpObs: Observer[Unit]
  ): Unit = {
    if (ev.defaultPrevented) return
    val target = ev.target.asInstanceOf[dom.Element]
    if (isTextInput(target)) return

    val key = ev.key
    // Help overlay swallows most keys — only Esc closes it, "?" toggles it.
    if (helpOpen) {
      key match {
        case "Escape" | "?" | "h" | "H" =>
          ev.preventDefault()
          toggleHelpObs.onNext(())
        case _ => ()
      }
      return
    }

    key match {
      case "Escape" =>
        ev.preventDefault()
        closeObs.onNext(())

      case "?" | "h" | "H" =>
        ev.preventDefault()
        toggleHelpObs.onNext(())

      case "ArrowRight" | "PageDown" =>
        ev.preventDefault()
        stepPage(cursor, pages, +1).foreach(goObs.onNext)

      case "ArrowLeft" | "PageUp" =>
        ev.preventDefault()
        stepPage(cursor, pages, -1).foreach(goObs.onNext)

      case "ArrowDown" | " " | "Spacebar" =>
        ev.preventDefault()
        stepItem(cursor, pages, +1).foreach(goObs.onNext)

      case "ArrowUp" =>
        ev.preventDefault()
        stepItem(cursor, pages, -1).foreach(goObs.onNext)

      case "Home" =>
        ev.preventDefault()
        if (pages.nonEmpty) goObs.onNext(Cursor(0, -1))

      case "End" =>
        ev.preventDefault()
        if (pages.nonEmpty) {
          val lastPage = pages.length - 1
          val lastItem = pages(lastPage).items.length - 1
          goObs.onNext(Cursor(lastPage, lastItem))
        }

      case "o" | "O" =>
        ev.preventDefault()
        goObs.onNext(cursor.copy(itemIdx = -1))

      case "Enter" =>
        if (cursor.isOverview) {
          ev.preventDefault()
          pages.lift(cursor.pageIdx).foreach { p =>
            if (p.items.nonEmpty) goObs.onNext(Cursor(cursor.pageIdx, 0))
          }
        }

      case _ => ()
    }
  }

  private def isTextInput(el: dom.Element): Boolean = {
    if (el == null) false
    else el.tagName match {
      case "INPUT" | "TEXTAREA" | "SELECT" => true
      case _ =>
        val ce = el.getAttribute("contenteditable")
        ce != null && ce != "false"
    }
  }

  private def clamp(c: Cursor, pages: Vector[Page]): Cursor = {
    if (pages.isEmpty) Cursor(0, -1)
    else {
      val p = math.max(0, math.min(pages.length - 1, c.pageIdx))
      val items = pages(p).items.length
      val i = if (c.itemIdx < 0) -1 else math.min(c.itemIdx, math.max(items - 1, -1))
      Cursor(p, i)
    }
  }

  private def stepPage(c: Cursor, pages: Vector[Page], delta: Int): Option[Cursor] = {
    if (pages.isEmpty) None
    else {
      val nextP = c.pageIdx + delta
      if (nextP < 0 || nextP >= pages.length) None
      else {
        val items = pages(nextP).items.length
        // Preserve item index where possible; if current is overview, land on overview.
        val nextI =
          if (c.itemIdx < 0) -1
          else if (items == 0) -1
          else math.min(c.itemIdx, items - 1)
        Some(Cursor(nextP, nextI))
      }
    }
  }

  private def stepItem(c: Cursor, pages: Vector[Page], delta: Int): Option[Cursor] = {
    if (pages.isEmpty) None
    else {
      val items = pages(c.pageIdx).items.length
      if (delta > 0) {
        if (c.isOverview) {
          if (items > 0) Some(c.copy(itemIdx = 0))
          else if (c.pageIdx + 1 < pages.length) Some(Cursor(c.pageIdx + 1, -1))
          else None
        } else if (c.itemIdx + 1 < items) Some(c.copy(itemIdx = c.itemIdx + 1))
        else if (c.pageIdx + 1 < pages.length) Some(Cursor(c.pageIdx + 1, -1))
        else None
      } else {
        if (c.isOverview) {
          if (c.pageIdx - 1 >= 0) {
            val prevItems = pages(c.pageIdx - 1).items.length
            Some(Cursor(c.pageIdx - 1, prevItems - 1))
          } else None
        } else if (c.itemIdx == 0) Some(c.copy(itemIdx = -1))
        else Some(c.copy(itemIdx = c.itemIdx - 1))
      }
    }
  }

  private def linearIndexOf(c: Cursor, pages: Vector[Page]): Int = {
    if (pages.isEmpty) 0
    else {
      var acc = 0
      var i   = 0
      while (i < c.pageIdx && i < pages.length) {
        acc += 1 + pages(i).items.length
        i += 1
      }
      acc += 1 // overview slide of current page
      if (c.itemIdx >= 0) acc += (c.itemIdx + 1)
      acc
    }
  }

  private def linearTotal(pages: Vector[Page]): Int =
    pages.foldLeft(0) { case (acc, p) => acc + 1 + p.items.length }
}
