package trail.reporting.browser.renderers

import com.raquo.laminar.api.L.{Mod as _, *}
import lui.style.*
import trail.reporting.browser.DataTable
import trail.reporting.schema.TableSpec

object TableRenderer {
  def apply(table: TableSpec): Modifier[HtmlElement] =
    DataTable(DataTable.table := table)
}
