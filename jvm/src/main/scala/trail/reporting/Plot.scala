package trail.reporting

import org.nspl.*
import org.nspl.awtrenderer.*
import trail.reporting.schema.DataItem

object Plot {

  def apply[K <: Renderable[K]](build: Build[K], width: Int = 800)(implicit
      r: Renderer[K, JavaRC]
  ): DataItem.PlotItem = {
    val bytes = svgToByteArray(build, width)
    DataItem.PlotItem(new String(bytes, "UTF-8"))
  }
}
