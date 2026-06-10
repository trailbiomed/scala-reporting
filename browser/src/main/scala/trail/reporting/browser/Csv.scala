package trail.reporting.browser

import trail.reporting.schema.Column

object Csv {

  def fromColumns(cols: Seq[Column], rowIndices: Seq[Int]): String = {
    val sb = new StringBuilder
    sb.append(cols.iterator.map(c => quote(c.label)).mkString(","))
    sb.append('\n')
    rowIndices.foreach { i =>
      sb.append(cols.iterator.map(c => quote(c.stringAt(i))).mkString(","))
      sb.append('\n')
    }
    sb.toString
  }

  private def quote(s: String): String = {
    val needs = s.contains(',') || s.contains('"') || s.contains('\n') || s.contains('\r')
    if (!needs) s
    else "\"" + s.replace("\"", "\"\"") + "\""
  }
}
