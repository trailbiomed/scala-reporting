package trail.reporting.browser

import com.raquo.laminar.api.L.{Mod as _, Button as _, table as tableTag, *}
import lui.*
import lui.components.*
import lui.style.*
import trail.reporting.schema.*

final class DataTable private[browser] (val root: HtmlElement) extends Component {
  private[browser] val tableVar: Var[TableSpec]                          = Var(TableSpec(Seq.empty))
  private[browser] val sortVar: Var[Option[(String, DataTable.SortDir)]] = Var(None)
  private[browser] val filtersVar: Var[Map[String, String]]              = Var(Map.empty)
  private[browser] val hiddenVar: Var[Set[String]]                       = Var(Set.empty)
  private[browser] val filenameVar: Var[String]                          = Var("data.csv")
  private[browser] val pageVar: Var[Int]                                 = Var(1)
  private[browser] val pageSizeVar: Var[Int]                             = Var(50)
}

object DataTable extends ComponentFactory[DataTable] {

  enum SortDir { case Asc, Desc }

  val table    = Prop.in[TableSpec, DataTable](_.tableVar)
  val filename = Prop.in[String, DataTable](_.filenameVar)
  val pageSize = Prop.in[Int, DataTable](_.pageSizeVar)

  private type VisibleCols = Seq[Column]

  private final case class BodyTheme(
      cellText:      Style,
      cellNullText:  Style,
      cellBorderRaw: String,
      stripedRow:    Style
  )

  override protected def build: DataTable = {
    val root = div()
    val el   = new DataTable(root)

    val visibleColsSig: Signal[VisibleCols] =
      Signal.combine(el.tableVar.signal, el.hiddenVar.signal).map { case (t, hidden) =>
        t.columns.filterNot(c => hidden(c.name))
      }.distinct

    val sortedFilteredSig: Signal[Seq[Int]] =
      Signal.combine(el.tableVar.signal, el.sortVar.signal, el.filtersVar.signal).map {
        case (t, sort, filters) =>
          val n = t.rowCount
          val allIndices = (0 until n).toArray
          val activeFilters = filters.iterator.flatMap { case (name, v) =>
            if (v.isEmpty) Iterator.empty
            else t.column(name).iterator.map(col => (col, v.toLowerCase))
          }.toArray
          val filtered =
            if (activeFilters.isEmpty) allIndices
            else allIndices.filter(i => rowMatches(i, activeFilters))
          sort match {
            case None => filtered.toIndexedSeq
            case Some((name, dir)) =>
              t.column(name) match {
                case None => filtered.toIndexedSeq
                case Some(col) =>
                  val sorted = filtered.sortWith((a, b) => col.compareAt(a, b) < 0)
                  dir match {
                    case SortDir.Asc  => sorted.toIndexedSeq
                    case SortDir.Desc => sorted.reverse.toIndexedSeq
                  }
              }
          }
      }

    val totalPagesSig: Signal[Int] =
      Signal.combine(sortedFilteredSig, el.pageSizeVar.signal).map { case (rs, ps) =>
        if (rs.isEmpty) 1 else math.max(1, (rs.size + ps - 1) / ps)
      }

    val clampedPageSig: Signal[Int] =
      Signal.combine(el.pageVar.signal, totalPagesSig).map { case (p, total) =>
        math.min(math.max(1, p), total)
      }

    val pagedRowsSig: Signal[Seq[Int]] =
      Signal.combine(sortedFilteredSig, clampedPageSig, el.pageSizeVar.signal).map {
        case (rs, page, ps) =>
          val from = (page - 1) * ps
          rs.slice(from, from + ps)
      }

    val countsSig: Signal[(Int, Int, Int, Int)] =
      Signal.combine(sortedFilteredSig, el.tableVar.signal, clampedPageSig, el.pageSizeVar.signal).map {
        case (rs, t, page, ps) =>
          val from = if (rs.isEmpty) 0 else (page - 1) * ps + 1
          val to   = math.min(rs.size, page * ps)
          (from, to, rs.size, t.rowCount)
      }

    root.amend(
      stack.col(spacing.md),
      toolbar(el, visibleColsSig, sortedFilteredSig, countsSig),
      tableEl(el, visibleColsSig, pagedRowsSig, sortedFilteredSig),
      paginationFooter(el, clampedPageSig, totalPagesSig),
      el.filtersVar.signal.changes.mapTo(1)        --> el.pageVar.writer,
      el.pageSizeVar.signal.changes.mapTo(1)       --> el.pageVar.writer,
      clampedPageSig.changes.distinct              --> el.pageVar.writer
    )
    el
  }

  private def paginationFooter(
      el: DataTable,
      clampedPageSig: Signal[Int],
      totalPagesSig: Signal[Int]
  ): HtmlElement =
    div(
      stack.between(spacing.md) ++ css.padding(spacing.xs, spacing.lg),
      span(
        typo.hint,
        child.text <-- Signal.combine(clampedPageSig, totalPagesSig).map { case (p, t) =>
          s"Page $p of $t"
        }
      ),
      child.maybe <-- totalPagesSig.map { total =>
        if (total <= 1) None
        else Some(
          Pagination(
            Pagination.page       <--> el.pageVar,
            Pagination.totalPages <--  totalPagesSig,
            Pagination.siblings   :=   1
          ).root
        )
      }
    )

  private def toolbar(
      el: DataTable,
      visibleColsSig: Signal[VisibleCols],
      sortedFilteredSig: Signal[Seq[Int]],
      countsSig: Signal[(Int, Int, Int, Int)]
  ): HtmlElement = {
    val exportBtn = Button(
      Button.label   := "Export CSV",
      Button.variant := Button.Variant.Secondary,
      Button.size    := Button.Size.Small
    )
    val clearBtn = Button(
      Button.label   := "Clear filters",
      Button.variant := Button.Variant.Ghost,
      Button.size    := Button.Size.Small
    )
    clearBtn.root.amend(
      clearBtn.clicks.mapTo(Map.empty[String, String]) --> el.filtersVar.writer
    )
    exportBtn.root.amend(
      exportBtn.clicks.compose(
        _.withCurrentValueOf(visibleColsSig, sortedFilteredSig, el.filenameVar.signal)
      ) --> Observer[(VisibleCols, Seq[Int], String)] { case (cols, rows, name) =>
        val csv = Csv.fromColumns(cols, rows)
        Downloads.fromText(csv, name, "text/csv")
      }
    )
    div(
      themed(t =>
        stack.between(spacing.md) ++
          css.padding(spacing.md, spacing.lg) ++
          css.background(t.surfaceDim) ++
          css.border(Length.px(1), BorderStyle.Solid, t.border) ++
          css.borderRadius(radius.md)
      ),
      div(
        stack.row(spacing.md) ++ css.alignItems("center"),
        span(typo.hint, child.text <-- countsSig.map { case (from, to, filtered, total) =>
          if (filtered == total) s"$from–$to of $filtered rows"
          else                   s"$from–$to of $filtered (filtered from $total)"
        }),
        child.maybe <-- el.filtersVar.signal.map { fs =>
          if (fs.values.exists(_.nonEmpty)) Some(clearBtn.root) else None
        }
      ),
      div(
        stack.row(spacing.md) ++ css.alignItems("center"),
        columnsMenu(el),
        exportBtn
      )
    )
  }

  private def columnsMenu(el: DataTable): HtmlElement = {
    val open = Var(false)
    val triggerBtn = Button(
      Button.label   := "Columns",
      Button.variant := Button.Variant.Ghost,
      Button.size    := Button.Size.Small
    )
    Popover(
      Popover.open      <--> open,
      Popover.placement := Popover.Placement.Bottom,
      Popover.trigger(triggerBtn),
      Popover.body(
        div(
          stack.col(spacing.sm) ++ css.raw("min-width", "200px") ++ css.padding(spacing.md),
          span(typo.eyebrow, "Visible columns"),
          children <-- el.tableVar.signal.map(_.columns).distinct.map { cols =>
            cols.map(c => columnToggle(c, el)).toList
          }
        )
      )
    ).root
  }

  private def columnToggle(col: Column, el: DataTable): HtmlElement = {
    val showing = Var(true)
    val box = Checkbox(
      Checkbox.label   := col.label,
      Checkbox.checked <--> showing
    )
    box.root.amend(
      el.hiddenVar.signal.map(h => !h.contains(col.name)).distinct --> showing.writer,
      showing.signal.changes.distinct.compose(_.withCurrentValueOf(el.hiddenVar.signal))
        .map { case (visible, hidden) =>
          if (visible) hidden - col.name else hidden + col.name
        } --> el.hiddenVar.writer
    )
    box.root
  }

  private def tableEl(
      el: DataTable,
      visibleColsSig: Signal[VisibleCols],
      pagedRowsSig: Signal[Seq[Int]],
      sortedFilteredSig: Signal[Seq[Int]]
  ): HtmlElement = {
    val bodyThemeSig: Signal[BodyTheme] = Theme.signal.map { t =>
      BodyTheme(
        cellText      = css.color(t.text),
        cellNullText  = css.color(t.textSubtle),
        cellBorderRaw = s"1px solid ${t.border.toCss}",
        stripedRow    = css.background(t.surfaceDim)
      )
    }
    div(
      themed(t =>
        css.border(Length.px(1), BorderStyle.Solid, t.border) ++
          css.borderRadius(radius.md) ++
          css.overflow("hidden")
      ),
      div(
        css.raw("overflow-x", "auto"),
        tableTag(
          themed(t =>
            css.width(Length.pct(100)) ++
              css.raw("border-collapse", "collapse") ++
              css.fontSize(fontSizes.lg) ++
              css.background(t.surface)
          ),
          thead(
            tr(
              children <-- visibleColsSig.map(_.map(col => headerCell(col, el)).toList)
            )
          ),
          tbody(
            children <-- Signal
              .combine(pagedRowsSig, visibleColsSig, bodyThemeSig, sortedFilteredSig, el.pageSizeVar.signal)
              .map { case (rows, cols, bt, allRows, ps) =>
                val real = rows.iterator.zipWithIndex.map { case (rowIdx, ri) =>
                  bodyRow(rowIdx, ri, cols, bt)
                }.toList
                val padCount =
                  if (allRows.size > ps) math.max(0, ps - rows.size) else 0
                val pad = List.fill(padCount)(placeholderRow(cols, bt))
                real ++ pad
              }
          )
        )
      ),
      child.maybe <-- Signal.combine(sortedFilteredSig, el.tableVar.signal).map { case (rows, t) =>
        if (rows.isEmpty)
          Some(
            div(
              themed(tt =>
                stack.centerAll ++
                  css.padding(spacing.xxxl) ++
                  css.color(tt.textMuted)
              ),
              if (t.rowCount == 0) "No data." else "No rows match the current filters."
            )
          )
        else None
      }
    )
  }

  private def headerCell(col: Column, el: DataTable): HtmlElement = {
    val hovered = Var(false)
    val isActive = el.sortVar.signal.map(_.exists(_._1 == col.name))
    th(
      themed(t =>
        css.padding(Length.zero) ++
          css.textAlign(TextAlign.Left) ++
          css.raw("border-bottom", s"1px solid ${t.border.toCss}") ++
          css.background(t.surface) ++
          css.position("sticky") ++
          css.raw("top", "0") ++
          css.raw("vertical-align", "top")
      ),
      div(
        stack.col(spacing.xs) ++ css.padding(spacing.md, spacing.lg),
        div(
          Signal.combine(hovered.signal, isActive).styled { case (t, (h, active)) =>
            val color =
              if (active) t.brand
              else if (h) t.text
              else        t.textMuted
            stack.row(spacing.xs) ++
              css.alignItems("center") ++
              css.cursor("pointer") ++
              css.color(color) ++
              css.fontWeight(if (active) FontWeight.SemiBold else FontWeight.Medium) ++
              css.raw("user-select", "none")
          },
          onMouseEnter.mapTo(true)  --> hovered.writer,
          onMouseLeave.mapTo(false) --> hovered.writer,
          onClick.mapTo(col.name).compose(
            _.withCurrentValueOf(el.sortVar.signal)
              .map { case (name, current) => DataTable.nextSort(current, name) }
          ) --> el.sortVar.writer,
          span(col.label),
          span(
            themed(_ => css.fontSize(fontSizes.md)),
            child.text <-- el.sortVar.signal.map {
              case Some((n, SortDir.Asc))  if n == col.name => "▲"
              case Some((n, SortDir.Desc)) if n == col.name => "▼"
              case _                                        => ""
            }
          )
        ),
        filterInput(col, el)
      )
    )
  }

  private def filterInput(col: Column, el: DataTable): HtmlElement = {
    val draft = Var("")
    val ti = TextInput(
      TextInput.placeholder := "filter…",
      TextInput.width       := Length.pct(100),
      TextInput.fontSize    := fontSizes.md,
      TextInput.value       <-- draft.signal,
      TextInput.value       --> draft.writer
    )
    ti.root.amend(
      el.filtersVar.signal.map(_.getOrElse(col.name, "")).distinct --> draft.writer,
      draft.signal.changes
        .compose(_.debounce(120).withCurrentValueOf(el.filtersVar.signal))
        .map { case (text, fs) =>
          if (text.isEmpty) fs - col.name else fs.updated(col.name, text)
        } --> el.filtersVar.writer
    )
    ti.root
  }

  private def placeholderRow(cols: VisibleCols, bt: BodyTheme): HtmlElement =
    tr(
      css.raw("visibility", "hidden"),
      aria.hidden := true,
      cols.map(_ =>
        td(
          css.padding(spacing.md, spacing.lg) ++
            css.raw("border-bottom", bt.cellBorderRaw),
          "\u00A0"
        )
      ).toList
    )

  private def bodyRow(rowIdx: Int, ri: Int, cols: VisibleCols, bt: BodyTheme): HtmlElement =
    tr(
      (if (ri % 2 == 1) bt.stripedRow else Style.empty),
      cols.map(col => bodyCell(col, rowIdx, bt)).toList
    )

  private def bodyCell(col: Column, rowIdx: Int, bt: BodyTheme): HtmlElement = {
    val alignStyle = if (col.isNumeric)
      css.textAlign(TextAlign.Right) ++ css.raw("font-variant-numeric", "tabular-nums")
    else Style.empty
    val colorStyle = if (col.isNullAt(rowIdx)) bt.cellNullText else bt.cellText
    val full =
      css.padding(spacing.md, spacing.lg) ++
        css.raw("border-bottom", bt.cellBorderRaw) ++
        colorStyle ++
        alignStyle
    td(full, col.stringAt(rowIdx))
  }

  private def rowMatches(rowIdx: Int, filters: Array[(Column, String)]): Boolean = {
    var i = 0
    while (i < filters.length) {
      val (col, needle) = filters(i)
      if (!col.stringAt(rowIdx).toLowerCase.contains(needle)) return false
      i += 1
    }
    true
  }

  private def nextSort(current: Option[(String, SortDir)], col: String): Option[(String, SortDir)] =
    current match {
      case Some((c, SortDir.Asc))  if c == col => Some((c, SortDir.Desc))
      case Some((c, SortDir.Desc)) if c == col => None
      case _                                   => Some((col, SortDir.Asc))
    }
}
