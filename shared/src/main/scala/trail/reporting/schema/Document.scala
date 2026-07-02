package trail.reporting.schema

final case class Document(
    title: String,
    pages: Seq[Page],
    createdAt: String,
    version: Option[String] = None,
    source: Option[SourceFile] = None,
    logo: Option[String] = None,
    footnote: Option[String] = None
)

final case class SourceFile(
    filename: String,
    mimeType: String,
    contentBase64: String
)

final case class Page(
    id: String,
    title: String,
    items: Seq[Item]
)

final case class Item(
    id: String,
    title: String,
    data: Seq[DataItem]
)

enum DataItem {
  case TextItem(content: String)
  case CodeItem(language: String, source: String)
  case TableItem(table: TableSpec)
  case PlotItem(svg: String)
}

final case class TableSpec(columns: Seq[Column]) {
  def rowCount: Int   = columns.headOption.map(_.length).getOrElse(0)
  def column(name: String): Option[Column] = columns.find(_.name == name)
  def indexOf(name: String): Int            = columns.indexWhere(_.name == name)
}

/** A single column. Values are stored column-major (one array per column) so the per-cell
  * wrapper allocation that a `Seq[Seq[CellValue]]` layout would require is gone, and the
  * JSON payload encodes each column as a flat array of native scalars. `nulls` is the
  * sparse set of row indices where the value is null/missing. */
sealed trait Column {
  def name:  String
  def label: String
  def length: Int
  def isNullAt(i: Int): Boolean
  def stringAt(i: Int): String
  def doubleAt(i: Int): Double
  def compareAt(i: Int, j: Int): Int
  def isNumeric: Boolean
}

final case class StringColumn(
    name:   String,
    label:  String,
    values: IndexedSeq[String],
    nulls:  Set[Int] = Set.empty
) extends Column {
  def length: Int                    = values.length
  def isNullAt(i: Int): Boolean      = nulls.contains(i)
  def stringAt(i: Int): String       = if (isNullAt(i)) "" else values(i)
  def doubleAt(i: Int): Double       =
    if (isNullAt(i)) Double.NaN else values(i).toDoubleOption.getOrElse(Double.NaN)
  def isNumeric: Boolean             = false
  def compareAt(i: Int, j: Int): Int = Column.nullsLastCompare(this, i, j) {
    values(i).compareTo(values(j))
  }
}

final case class NumberColumn(
    name:   String,
    label:  String,
    values: IndexedSeq[Double],
    nulls:  Set[Int] = Set.empty
) extends Column {
  def length: Int                    = values.length
  def isNullAt(i: Int): Boolean      = nulls.contains(i)
  def stringAt(i: Int): String       = if (isNullAt(i)) "" else Column.formatNumber(values(i))
  def doubleAt(i: Int): Double       = if (isNullAt(i)) Double.NaN else values(i)
  def isNumeric: Boolean             = true
  def compareAt(i: Int, j: Int): Int = Column.nullsLastCompare(this, i, j) {
    java.lang.Double.compare(values(i), values(j))
  }
}

final case class IntegerColumn(
    name:   String,
    label:  String,
    values: IndexedSeq[Long],
    nulls:  Set[Int] = Set.empty
) extends Column {
  def length: Int                    = values.length
  def isNullAt(i: Int): Boolean      = nulls.contains(i)
  def stringAt(i: Int): String       = if (isNullAt(i)) "" else values(i).toString
  def doubleAt(i: Int): Double       = if (isNullAt(i)) Double.NaN else values(i).toDouble
  def isNumeric: Boolean             = true
  def compareAt(i: Int, j: Int): Int = Column.nullsLastCompare(this, i, j) {
    java.lang.Long.compare(values(i), values(j))
  }
}

final case class BoolColumn(
    name:   String,
    label:  String,
    values: IndexedSeq[Boolean],
    nulls:  Set[Int] = Set.empty
) extends Column {
  def length: Int                    = values.length
  def isNullAt(i: Int): Boolean      = nulls.contains(i)
  def stringAt(i: Int): String       = if (isNullAt(i)) "" else values(i).toString
  def doubleAt(i: Int): Double       =
    if (isNullAt(i)) Double.NaN else if (values(i)) 1.0 else 0.0
  def isNumeric: Boolean             = false
  def compareAt(i: Int, j: Int): Int = Column.nullsLastCompare(this, i, j) {
    java.lang.Boolean.compare(values(i), values(j))
  }
}

object Column {
  private[schema] inline def nullsLastCompare(c: Column, i: Int, j: Int)(inline cmp: => Int): Int = {
    val ni = c.isNullAt(i)
    val nj = c.isNullAt(j)
    if (ni && nj) 0
    else if (ni)  1
    else if (nj) -1
    else          cmp
  }

  private[schema] def formatNumber(d: Double): String =
    if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) d.toString
    else if (d == d.toLong.toDouble && math.abs(d) < 1e15)            d.toLong.toString
    else                                                              d.toString
}
