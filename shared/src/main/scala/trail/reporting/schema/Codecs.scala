package trail.reporting.schema

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

object Codecs {

  given JsonValueCodec[Document] = JsonCodecMaker.make[Document](
    CodecMakerConfig
      .withDiscriminatorFieldName(Some("type"))
      .withTransientEmpty(false)
      .withTransientNone(false)
  )
}
