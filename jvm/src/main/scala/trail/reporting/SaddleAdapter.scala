package trail.reporting

import org.saddle.{Frame, Series, Vec}
import org.saddle.scalar.ScalarTag
import trail.reporting.schema.*

/** Convert a Saddle frame whose value type is [[T]] into a columnar [[TableSpec]].
  * Resolved at compile time by the DSL's `.frame(f)` extension, so unsupported value
  * types yield a missing-instance error instead of a runtime failure. */
trait FrameConvert[T] {
  def apply[RX, CX](frame: Frame[RX, CX, T]): TableSpec
}

object FrameConvert {
  given FrameConvert[Double]  = new FrameConvert[Double]  { def apply[RX, CX](f: Frame[RX, CX, Double])  = SaddleAdapter.fromFrame(f)       }
  given FrameConvert[Long]    = new FrameConvert[Long]    { def apply[RX, CX](f: Frame[RX, CX, Long])    = SaddleAdapter.fromLongFrame(f)   }
  given FrameConvert[String]  = new FrameConvert[String]  { def apply[RX, CX](f: Frame[RX, CX, String])  = SaddleAdapter.fromStringFrame(f) }
  given FrameConvert[Boolean] = new FrameConvert[Boolean] { def apply[RX, CX](f: Frame[RX, CX, Boolean]) = SaddleAdapter.fromBoolFrame(f)   }
}

object SaddleAdapter {

  def fromFrame[RX, CX](frame: Frame[RX, CX, Double]): TableSpec =
    TableSpec(framedColumns(frame, numberColumn[RX]))

  def fromLongFrame[RX, CX](frame: Frame[RX, CX, Long]): TableSpec =
    TableSpec(framedColumns(frame, integerColumn[RX]))

  def fromStringFrame[RX, CX](frame: Frame[RX, CX, String]): TableSpec =
    TableSpec(framedColumns(frame, stringColumn[RX]))

  def fromBoolFrame[RX, CX](frame: Frame[RX, CX, Boolean]): TableSpec =
    TableSpec(framedColumns(frame, boolColumn[RX]))

  def numberColumn[RX](name: String, label: String, series: Series[RX, Double]): NumberColumn = {
    val arr = series.toVec.toArray
    NumberColumn(name, label, arr.toIndexedSeq, nullsOf(arr, ScalarTag.stDouble))
  }

  def integerColumn[RX](name: String, label: String, series: Series[RX, Long]): IntegerColumn = {
    val arr = series.toVec.toArray
    IntegerColumn(name, label, arr.toIndexedSeq, nullsOf(arr, ScalarTag.stLong))
  }

  def stringColumn[RX](name: String, label: String, series: Series[RX, String]): StringColumn = {
    val arr = series.toVec.toArray
    val st  = ScalarTag.stString
    val nulls = nullsOf(arr, st)
    val safe = Array.tabulate(arr.length)(i => if (nulls(i)) "" else arr(i))
    StringColumn(name, label, safe.toIndexedSeq, nulls)
  }

  def boolColumn[RX](name: String, label: String, series: Series[RX, Boolean]): BoolColumn = {
    val arr = series.toVec.toArray
    BoolColumn(name, label, arr.toIndexedSeq, Set.empty)
  }

  private def framedColumns[RX, CX, T, C <: Column](
      frame: Frame[RX, CX, T],
      build: (String, String, Series[RX, T]) => C
  ): Seq[C] = {
    val colIx = frame.colIx
    (0 until frame.numCols).map { i =>
      val key = colIx.raw(i).toString
      build(key, key, frame.colAt(i))
    }
  }

  private def nullsOf[T](arr: Array[T], st: ScalarTag[T]): Set[Int] = {
    val b = Set.newBuilder[Int]
    var i = 0
    while (i < arr.length) {
      if (st.isMissing(arr(i))) b += i
      i += 1
    }
    b.result()
  }
}
