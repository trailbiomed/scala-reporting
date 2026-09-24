---
name: trail-reporting
description: Use when building single-file HTML reports with the trail.reporting Scala library — `Document` → `Page` → `Item` → `DataItem`, emitted via `Report.write(doc, path)`.
---

# trail-reporting

Fluent, immutable DSL that builds a `Document` and serialises it as one self-contained HTML file (JSON payload + inlined Scala.js bundle, no network needed to view).

## Imports
JVM: `import trail.reporting.*, trail.reporting.dsl.*, trail.reporting.schema.*`. Browser (Scala.js) uses the same DSL minus JVM-only pieces (Saddle `.frame`, nspl `.plot`, `withSource(Path)`).

## Shape
`document(title).page(id, title, item(...), item(...))`. Items chain data: `item(id, title).text(...).code(lang, src).table(spec).frame(saddleFrame).plot(nsplBuild).image(png).pdb(pdb, style, color, height).custom(kind, payload)`. Every call returns a fresh value. Text accepts simple markdown.

## Document knobs
`.withVersion`, `.withSource(Path)`, `.withLogo(svg)`, `.withFootnote`, `.withLayout(DocumentLayout.HorizontalTabs | VerticalCards)` — or shorthand `.verticalNavigator`.

## Page knobs
`.withName`, `.withDescription`, `.withTags("k" -> "v", ...)`, `.withItemMenu(Inline | Popover | Hidden)`.

## Tables
`TableSpec(Seq(StringColumn | NumberColumn | IntegerColumn | BoolColumn))` — column-major, `nulls: Set[Int]` marks missing rows. `SaddleAdapter.numberColumn/boolColumn` adapt Saddle `Series`.

## Images
`.image(path)` / `.image(bytes, mimeType, alt)` (JVM) or `.image(base64, mimeType, alt)` (browser) embeds a raster image (PNG, JPEG, …) as a base64 `data:` URI `<img>` — for figures you already have as pixels, in contrast to `.plot`, which embeds vector SVG. `mimeType` defaults to `image/png`.

## Emit
`Report.write(doc, Paths.get("report.html"))` — single file, opens offline. `Report.render(doc, out)` streams to any `OutputStream`.

Custom widgets: `custom(kind, payload)` pairs with a browser-side renderer registered via `trail.reporting.browser.mount(root, doc, customRenderers = ...)`. Full example at `example/src/main/scala/trail/reporting/example/Main.scala`.
