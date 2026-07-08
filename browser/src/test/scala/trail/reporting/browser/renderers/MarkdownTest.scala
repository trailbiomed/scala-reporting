package trail.reporting.browser.renderers

import Markdown.{Block, Inline}

class MarkdownTest extends munit.FunSuite {

  test("plain text becomes a single paragraph") {
    assertEquals(
      Markdown.parse("hello world"),
      Seq(Block.Paragraph(Seq(Inline.Plain("hello world"))))
    )
  }

  test("blank line splits paragraphs") {
    assertEquals(
      Markdown.parse("first\n\nsecond"),
      Seq(
        Block.Paragraph(Seq(Inline.Plain("first"))),
        Block.Paragraph(Seq(Inline.Plain("second")))
      )
    )
  }

  test("single newlines collapse to spaces within a paragraph") {
    assertEquals(
      Markdown.parse("first line\nsecond line"),
      Seq(Block.Paragraph(Seq(Inline.Plain("first line second line"))))
    )
  }

  test("ATX headings map to level 1/2/3") {
    assertEquals(
      Markdown.parse("# H1\n\n## H2\n\n### H3"),
      Seq(
        Block.Heading(1, Seq(Inline.Plain("H1"))),
        Block.Heading(2, Seq(Inline.Plain("H2"))),
        Block.Heading(3, Seq(Inline.Plain("H3")))
      )
    )
  }

  test("dash bullets group into a single Bullets block") {
    assertEquals(
      Markdown.parse("- one\n- two\n- three"),
      Seq(Block.Bullets(Seq(
        Seq(Inline.Plain("one")),
        Seq(Inline.Plain("two")),
        Seq(Inline.Plain("three"))
      )))
    )
  }

  test("star bullets are equivalent to dash bullets") {
    assertEquals(
      Markdown.parse("* one\n* two"),
      Seq(Block.Bullets(Seq(
        Seq(Inline.Plain("one")),
        Seq(Inline.Plain("two"))
      )))
    )
  }

  test("fenced code block preserves language and body") {
    assertEquals(
      Markdown.parse("```scala\nval x = 1\nval y = 2\n```"),
      Seq(Block.CodeBlock("scala", "val x = 1\nval y = 2"))
    )
  }

  test("fenced code block without a language") {
    assertEquals(
      Markdown.parse("```\nplain\n```"),
      Seq(Block.CodeBlock("", "plain"))
    )
  }

  test("unclosed code fence still captures the remainder") {
    assertEquals(
      Markdown.parse("```\nno close"),
      Seq(Block.CodeBlock("", "no close"))
    )
  }

  test("bold and italic inline") {
    assertEquals(
      Markdown.parseInline("plain **bold** more *italic* end"),
      Seq(
        Inline.Plain("plain "),
        Inline.Bold("bold"),
        Inline.Plain(" more "),
        Inline.Italic("italic"),
        Inline.Plain(" end")
      )
    )
  }

  test("inline code and link") {
    assertEquals(
      Markdown.parseInline("see `foo` at [docs](https://example.com)"),
      Seq(
        Inline.Plain("see "),
        Inline.CodeSpan("foo"),
        Inline.Plain(" at "),
        Inline.Link("docs", "https://example.com")
      )
    )
  }

  test("unmatched delimiter renders literally") {
    assertEquals(
      Markdown.parseInline("no close *here"),
      Seq(Inline.Plain("no close *here"))
    )
  }

  test("backslash escapes the next character") {
    assertEquals(
      Markdown.parseInline("literal \\* asterisk"),
      Seq(Inline.Plain("literal * asterisk"))
    )
  }

  test("empty emphasis body is treated as literal") {
    assertEquals(
      Markdown.parseInline("empty **** here"),
      Seq(Inline.Plain("empty **** here"))
    )
  }

  test("mixed blocks: heading, paragraph, list, code") {
    val md =
      """# Overview
        |
        |Some **bold** text with `code`.
        |
        |- one
        |- two
        |
        |```scala
        |val x = 1
        |```""".stripMargin
    assertEquals(
      Markdown.parse(md),
      Seq(
        Block.Heading(1, Seq(Inline.Plain("Overview"))),
        Block.Paragraph(Seq(
          Inline.Plain("Some "),
          Inline.Bold("bold"),
          Inline.Plain(" text with "),
          Inline.CodeSpan("code"),
          Inline.Plain(".")
        )),
        Block.Bullets(Seq(
          Seq(Inline.Plain("one")),
          Seq(Inline.Plain("two"))
        )),
        Block.CodeBlock("scala", "val x = 1")
      )
    )
  }

  test("empty input produces no blocks") {
    assertEquals(Markdown.parse(""), Seq.empty[Block])
    assertEquals(Markdown.parseInline(""), Seq.empty[Inline])
  }
}
