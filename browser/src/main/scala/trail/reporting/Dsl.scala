package trail.reporting

import scala.scalajs.js
import trail.reporting.schema.*

/** JS-side counterpart of the JVM `trail.reporting.dsl` — a fluent, immutable builder
  * for [[Document]] values from Scala.js code. Mirrors the JVM DSL minus the
  * saddle / nspl / `java.nio.file.Path` bits, which have no meaning in the browser.
  *
  * A client's Scala.js project depends on `trail-reporting-browser`, imports
  * `trail.reporting.dsl.*`, and can build a Document identically to the JVM API:
  *
  * {{{
  *   import trail.reporting.dsl.*
  *   import trail.reporting.browser.mount
  *
  *   val doc = document("Sample")
  *     .withVersion("0.1.0")
  *     .page("overview", "Overview",
  *       item("intro", "Introduction").text("Hello from Scala.js")
  *     )
  *
  *   mount(dom.document.getElementById("root"), doc)
  * }}}
  */
object dsl {

  def document(title: String): Document =
    Document(title = title, pages = Seq.empty, createdAt = new js.Date().toISOString())

  def page(id: String, title: String, items: Item*): Page =
    Page(id, title, items)

  def item(id: String, title: String, data: DataItem*): Item =
    Item(id, title, data)

  def text(content: String): DataItem                  = DataItem.TextItem(content)
  def code(language: String, source: String): DataItem = DataItem.CodeItem(language, source)
  def table(spec: TableSpec): DataItem                 = DataItem.TableItem(spec)

  def pdb(content: String, style: PdbStyle, color: PdbColor, height: Int, background: String): DataItem =
    DataItem.PdbItem(content, style, color, height, background)

  def pdb(content: String): DataItem =
    pdb(content, PdbStyle.Cartoon, PdbColor.SsSuccession, 480, "white")

  /** Client-defined widget. `kind` selects a renderer registered via
    * `trail.reporting.browser.mount(..., customRenderers = ...)`; `payload` is arbitrary
    * text (typically JSON) that the renderer parses. */
  def custom(kind: String, payload: String): DataItem = DataItem.CustomItem(kind, payload)

  extension (doc: Document) {
    def withVersion(v: String): Document     = doc.copy(version = Some(v))
    def withSource(sf: SourceFile): Document = doc.copy(source = Some(sf))
    def withCreatedAt(ts: String): Document  = doc.copy(createdAt = ts)
    def withLogo(svg: String): Document      = doc.copy(logo = Some(svg))
    def withFootnote(text: String): Document = doc.copy(footnote = Some(text))
    def withPages(ps: Page*): Document       = doc.copy(pages = doc.pages ++ ps)

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
  }
}
