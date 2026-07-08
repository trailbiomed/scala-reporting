package trail.reporting

import java.nio.file.Path
import org.saddle.Frame
import trail.reporting.schema.*

/** Fluent, immutable construction for [[Document]] and friends. Every method returns a
  * fresh value — there is no shared mutable state.
  *
  * {{{
  *   import trail.reporting.*
  *   import trail.reporting.dsl.*
  *
  *   val doc = document("Sample Report")
  *     .withVersion("0.2.0")
  *     .withSource(path)
  *     .page("overview", "Overview",
  *       item("intro", "Introduction").text("…").code("scala", "…"),
  *       item("metrics", "Cohort").text("…").frame(myFrame)
  *     )
  * }}}
  */
object dsl {

  def document(title: String): Document =
    Document(title = title, pages = Seq.empty, createdAt = Report.nowIso())

  def page(id: String, title: String, items: Item*): Page =
    Page(id, title, items)

  def item(id: String, title: String, data: DataItem*): Item =
    Item(id, title, data)

  def text(content: String): DataItem                  = DataItem.TextItem(content)
  def code(language: String, source: String): DataItem = DataItem.CodeItem(language, source)
  def table(spec: TableSpec): DataItem                 = DataItem.TableItem(spec)

  /** Client-defined widget. `kind` selects a browser-side renderer registered via
    * `trail.reporting.browser.mount(..., customRenderers = ...)`; `payload` is arbitrary
    * text (typically JSON) that the renderer parses. */
  def custom(kind: String, payload: String): DataItem = DataItem.CustomItem(kind, payload)

  /** Render a PDB structure with bio-pv (WebGL). See [[PdbStyle]] / [[PdbColor]] for options. */
  def pdb(content: String, style: PdbStyle, color: PdbColor, height: Int, background: String): DataItem =
    DataItem.PdbItem(content, style, color, height, background)
  
  def pdb(content: String): DataItem =
    pdb(content, PdbStyle.Cartoon, PdbColor.SsSuccession, 480, "white")

  def frame[RX, CX, T](f: Frame[RX, CX, T])(using fc: FrameConvert[T]): DataItem =
    DataItem.TableItem(fc(f))

  def plot[K <: org.nspl.Renderable[K]](build: org.nspl.Build[K], width: Int)(implicit
      r: org.nspl.Renderer[K, org.nspl.JavaRC]
  ): DataItem = Plot(build, width)

  def plot[K <: org.nspl.Renderable[K]](build: org.nspl.Build[K])(implicit
      r: org.nspl.Renderer[K, org.nspl.JavaRC]
  ): DataItem = Plot(build)

  extension (doc: Document) {
    def withVersion(v: String): Document     = doc.copy(version = Some(v))
    def withSource(sf: SourceFile): Document = doc.copy(source = Some(sf))
    def withSource(path: Path): Document     = doc.copy(source = Some(Report.sourceFromPath(path)))
    def withCreatedAt(ts: String): Document  = doc.copy(createdAt = ts)
    def withLogo(svg: String): Document      = doc.copy(logo = Some(svg))
    def withFootnote(text: String): Document = doc.copy(footnote = Some(text))
    def withPages(ps: Page*): Document       = doc.copy(pages = doc.pages ++ ps)

    def withLayout(l: DocumentLayout): Document = doc.copy(layout = l)

    def verticalNavigator: Document = doc.copy(layout = DocumentLayout.VerticalCards)

    def page(id: String, title: String, items: Item*): Document =
      doc.copy(pages = doc.pages :+ Page(id, title, items))

    def page(p: Page): Document = doc.copy(pages = doc.pages :+ p)
  }

  extension (p: Page) {
    def withItems(is: Item*): Page = p.copy(items = p.items ++ is)

    def item(id: String, title: String, data: DataItem*): Page =
      p.copy(items = p.items :+ Item(id, title, data))

    def item(i: Item): Page = p.copy(items = p.items :+ i)

    def withDescription(text: String): Page = p.copy(description = Some(text))

    def withName(text: String): Page = p.copy(name = Some(text))

    def withTags(ts: (String, String)*): Page =
      p.copy(tags = p.tags ++ ts.map((k, v) => PageTag(k, v)))

    def withTag(name: String, value: String): Page =
      p.copy(tags = p.tags :+ PageTag(name, value))

    def withItemMenu(mode: PageItemMenu): Page = p.copy(itemMenu = mode)
  }

  extension (i: Item) {
    def add(d: DataItem): Item                       = i.copy(data = i.data :+ d)
    def withData(ds: DataItem*): Item                = i.copy(data = i.data ++ ds)
    def text(content: String): Item                  = i.add(DataItem.TextItem(content))
    def code(language: String, source: String): Item = i.add(DataItem.CodeItem(language, source))
    def table(spec: TableSpec): Item                 = i.add(DataItem.TableItem(spec))

    def pdb(
        content:    String,
        style:      PdbStyle = PdbStyle.Cartoon,
        color:      PdbColor = PdbColor.SsSuccession,
        height:     Int      = 480,
        background: String   = "white"
    ): Item = i.add(DataItem.PdbItem(content, style, color, height, background))

    def custom(kind: String, payload: String): Item = i.add(DataItem.CustomItem(kind, payload))

    def frame[RX, CX, T](f: Frame[RX, CX, T])(using fc: FrameConvert[T]): Item =
      i.add(DataItem.TableItem(fc(f)))

    def plot[K <: org.nspl.Renderable[K]](build: org.nspl.Build[K], width: Int = 800)(implicit
        r: org.nspl.Renderer[K, org.nspl.JavaRC]
    ): Item = i.add(Plot(build, width))
  }
}
