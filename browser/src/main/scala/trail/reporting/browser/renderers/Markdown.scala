package trail.reporting.browser.renderers

import scala.collection.mutable

/** Minimal Markdown flavor for [[trail.reporting.schema.DataItem.TextItem]] content.
  *
  * Supported:
  *   - ATX headings `#`, `##`, `###`
  *   - Bullet lists (`- ` or `* ` at line start, contiguous run)
  *   - Fenced code blocks (```` ``` ```` with optional language)
  *   - Inline emphasis `**bold**`, `*italic*`, `` `code` ``, `[label](url)`
  *   - Paragraphs separated by blank lines; single newlines within a paragraph collapse to spaces
  *   - `\` escapes the next character
  *
  * Unmatched delimiters render literally. No nesting of inline styles.
  */
private[browser] object Markdown {

  enum Inline {
    case Plain(text: String)
    case Bold(text: String)
    case Italic(text: String)
    case CodeSpan(text: String)
    case Link(label: String, href: String)
  }

  enum Block {
    case Heading(level: Int, inlines: Seq[Inline])
    case Paragraph(inlines: Seq[Inline])
    case Bullets(items: Seq[Seq[Inline]])
    case CodeBlock(language: String, source: String)
  }

  def parse(source: String): Seq[Block] = {
    val lines = source.split("\n", -1).toIndexedSeq
    val out   = mutable.ArrayBuffer.empty[Block]
    var i     = 0
    while (i < lines.length) {
      val line = lines(i)
      if (line.trim.isEmpty) {
        i += 1
      } else if (line.startsWith("```")) {
        val lang   = line.stripPrefix("```").trim
        val body   = new StringBuilder
        var closed = false
        i += 1
        while (i < lines.length && !closed) {
          if (lines(i).startsWith("```")) { closed = true; i += 1 }
          else {
            if (body.nonEmpty) body.append('\n')
            body.append(lines(i))
            i += 1
          }
        }
        out += Block.CodeBlock(lang, body.toString)
      } else headingLevel(line) match {
        case Some((lvl, rest)) =>
          out += Block.Heading(lvl, parseInline(rest))
          i += 1
        case None if isBullet(line) =>
          val items = mutable.ArrayBuffer.empty[Seq[Inline]]
          while (i < lines.length && isBullet(lines(i))) {
            items += parseInline(lines(i).drop(2))
            i += 1
          }
          out += Block.Bullets(items.toSeq)
        case None =>
          val buf = new StringBuilder
          while (
            i < lines.length && lines(i).trim.nonEmpty
              && headingLevel(lines(i)).isEmpty
              && !isBullet(lines(i))
              && !lines(i).startsWith("```")
          ) {
            if (buf.nonEmpty) buf.append(' ')
            buf.append(lines(i))
            i += 1
          }
          out += Block.Paragraph(parseInline(buf.toString))
      }
    }
    out.toSeq
  }

  private def headingLevel(line: String): Option[(Int, String)] =
    if (line.startsWith("### ")) Some((3, line.drop(4)))
    else if (line.startsWith("## ")) Some((2, line.drop(3)))
    else if (line.startsWith("# ")) Some((1, line.drop(2)))
    else None

  private def isBullet(line: String): Boolean =
    line.startsWith("- ") || line.startsWith("* ")

  def parseInline(source: String): Seq[Inline] = {
    val out   = mutable.ArrayBuffer.empty[Inline]
    val plain = new StringBuilder

    def flushPlain(): Unit =
      if (plain.nonEmpty) { out += Inline.Plain(plain.toString); plain.setLength(0) }

    // Match `open...close` with a non-empty body starting at `pos`.
    // Returns (inline, chars-consumed).
    def delim(pos: Int, open: String, close: String, wrap: String => Inline): Option[(Inline, Int)] = {
      if (!source.startsWith(open, pos)) return None
      val bodyStart = pos + open.length
      val end       = source.indexOf(close, bodyStart)
      if (end <= bodyStart) return None
      Some((wrap(source.substring(bodyStart, end)), end + close.length - pos))
    }

    // Match `[label](href)` (non-empty label and href) starting at `pos`.
    def link(pos: Int): Option[(Inline, Int)] = {
      if (source.charAt(pos) != '[') return None
      val labelStart   = pos + 1
      val closeBracket = source.indexOf(']', labelStart)
      if (closeBracket <= labelStart) return None
      if (!source.startsWith("(", closeBracket + 1)) return None
      val hrefStart  = closeBracket + 2
      val closeParen = source.indexOf(')', hrefStart)
      if (closeParen <= hrefStart) return None
      val label = source.substring(labelStart, closeBracket)
      val href  = source.substring(hrefStart, closeParen)
      Some((Inline.Link(label, href), closeParen + 1 - pos))
    }

    def tokenAt(pos: Int): Option[(Inline, Int)] =
      delim(pos, "**", "**", Inline.Bold(_))
        .orElse(delim(pos, "*", "*", Inline.Italic(_)))
        .orElse(delim(pos, "`", "`", Inline.CodeSpan(_)))
        .orElse(link(pos))

    var pos = 0
    while (pos < source.length) {
      val c = source.charAt(pos)
      if (c == '\\' && pos + 1 < source.length) {
        plain.append(source.charAt(pos + 1))
        pos += 2
      } else tokenAt(pos) match {
        case Some((inline, consumed)) =>
          flushPlain()
          out += inline
          pos += consumed
        case None =>
          plain.append(c)
          pos += 1
      }
    }
    flushPlain()
    out.toSeq
  }
}
