# Plot draw shapes

Temporary notes for the drawing payload a front end needs. They will move into the `plot.inz*` Roxygen docs once those methods return these objects.

`schemaVersion` starts at 1. Point size, line width, fonts, and whether a bar axis is labelled in counts or percentages stay on the client. Colour scales are expected to live on the `PlotController` rather than in this payload. Whether two linked plots may colour by different variables is still open, so this file does not put a shared colour domain on the envelope. Inference intervals and the missing-value footnote stay off every shape below.

The TypeScript blocks are the intended JSON. The `ts_list()` blocks are a draft RserveTS result type for the same object. A named `ts_list()` currently requires every name to be present, so a `ts_optional()` field is sent as `undefined` rather than omitted. `ts_list(element)` (one unnamed type) is a JSON array of that element. `ts_numeric(2L)` is a length-2 numeric vector.

## Envelope

Every plot uses the same envelope. `panels` is always a list, including when there is only one panel, so a facet adds panels without changing what `data` means.

```ts
type Plot = {
  schemaVersion: 1
  type: string
  variables: {
    v1: string
    v2?: string
    s1?: string
    s2?: string
    colby?: string
  }
  capabilities: {
    idSpace: "row" | "plotLocal" | "none"
  }
  layout: {
    matrix: boolean
    s1Levels?: string[]
    s2Levels?: string[]
  }
  panels: Panel[]
}
```

```r
plot_variables <- ts_list(
  v1 = ts_character(1L),
  v2 = ts_optional(ts_character(1L)),
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  colby = ts_optional(ts_character(1L))
)

# `panel` is the per-type list below.
plot_result <- ts_list(
  schemaVersion = ts_integer(1L),
  type = ts_character(1L),
  variables = plot_variables,
  capabilities = ts_list(
    idSpace = ts_character(1L) # "row" | "plotLocal" | "none"
  ),
  layout = ts_list(
    matrix = ts_logical(1L),
    s1Levels = ts_optional(ts_character(0L)),
    s2Levels = ts_optional(ts_character(0L))
  ),
  panels = ts_list(panel)
)
```

`type` is the plot that was requested, and it has to be valid for those variables. A one-variable factor can be `"bar"`. A numeric variable can be `"dot"` or `"hist"`. Two numerics can be `"scatter"`, `"hex"`, or `"grid"`. An invalid combination is an error. R does not substitute another type: a survey dot is not returned as a histogram, and a large scatter is not returned as a hex.

`capabilities.idSpace` says what an id in the payload means. `"row"` is a shared row id (scatter `point.order`). `"plotLocal"` is reserved for a later secure plot, where the handle is meaningful only to R. `"none"` means this plot has nothing to brush by row: bars, histograms, hexes, and grids. Highlighting those is a round-trip. Hex and grid cells do not carry row ids.

| Field | Meaning |
|-------|---------|
| `v1`, `v2` | Variable 1 and Variable 2. The widget already sends these as `x` and `y`. |
| `s1`, `s2` | Subset variables. The widget already sends these as `subset1` and `subset2`, which `iNZightPlot` takes as `g1` and `g2`. |
| `colby` | Segmented bar only. Not `v2`. |
| `s1` level | `subset1Level`. `_MULTI` returns one panel per level. A named level returns that panel only. |
| `s2` level | `subset2Level`. `_ALL` means no second split. `_MULTI` returns the `s1` by `s2` panels. |

The level list for the subset dropdown stays on the existing levels call. `layout` is the plot’s own axis order, which that call does not provide.

`layout.matrix` is true only for an `s1` by `s2` matrix. R lays that out as `s2` rows by `s1` columns, in factor order. `s1Levels` is the column order when `s1` is faceting, and is omitted otherwise. `s2Levels` is the row order when `matrix` is true, and is omitted otherwise. A single `s1` multi is `matrix: false` with `s1Levels` set, and the client chooses the wrap. An empty level stays in these arrays even when no panel carries it. `panels` stays a flat list.

A panel carries `s1` and `s2` only when that variable is faceting. Inside a panel, a second variable (`v2`) is groups or series, not another panel.

## Bar, one variable

`type` is `"bar"`. This is `inzplot(~Species, data = iris)`: one categorical variable, no second variable, no facet.

`count` is `tab`. `proportion` is `phat`. `total` is `ntotal`, once per panel. On a survey design, `tab` comes from `svytable` and `phat` from `svymean`, so the client uses `proportion` as given and does not divide `count` by `total`.

```ts
type BarPanel = {
  s1?: string
  s2?: string
  total: number
  data: Array<{
    label: string
    count: number
    proportion: number
  }>
}
```

```r
bar_datum <- ts_list(
  label = ts_character(1L),
  count = ts_numeric(1L),
  proportion = ts_numeric(1L)
)

bar_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  total = ts_numeric(1L),
  data = ts_list(bar_datum)
)
```

```ts
{
  schemaVersion: 1,
  type: "bar",
  variables: { v1: "Species" },
  panels: [
    {
      total: 150,
      data: [
        { label: "setosa", count: 50, proportion: 1 / 3 },
        { label: "versicolor", count: 50, proportion: 1 / 3 },
        { label: "virginica", count: 50, proportion: 1 / 3 }
      ]
    }
  ]
}
```

With `s1` set and `_MULTI`, the same `data` arrays are returned as one panel per level, and each panel includes its `s1` value.

## Bar, two variables

Still `type: "bar"`. `v1` is the axis cluster (one level of `x`). `v2` is the side-by-side series inside the cluster (levels of `y`). `count` is `tab[y, x]`. `proportion` is the share of that `x` **within that `y`**. On a survey, `proportion` is `svymean` of `x` by `y` and must not be recomputed from the counts. Those proportions are taken before any zoom and are not renormalised, so a series in a zoomed payload need not sum to 1.

`seriesTotals` is the sum of each `v2` level across every `v1` level, in the same units as `count`, computed before zoom drops columns. Relative bar widths use these totals. Summing the visible `count`s is wrong once a zoom has removed columns: the widths stay at the pre-zoom totals. When every `v1` level is present, `seriesTotals` matches those sums. Widths are equal when `bar.counts` is set or `bar.relative.width` is not. `widths`, `edges`, and `nn` are not sent.

```ts
type BarTwoWayPanel = {
  s1?: string
  s2?: string
  total: number
  seriesTotals: Array<{ label: string; total: number }>
  data: Array<{
    label: string
    series: Array<{
      label: string
      count: number
      proportion: number
    }>
  }>
}
```

```r
bar_series <- ts_list(
  label = ts_character(1L),
  count = ts_numeric(1L),
  proportion = ts_numeric(1L)
)

bar_cluster <- ts_list(
  label = ts_character(1L),
  series = ts_list(bar_series)
)

bar_series_total <- ts_list(
  label = ts_character(1L),
  total = ts_numeric(1L)
)

bar_twoway_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  total = ts_numeric(1L),
  seriesTotals = ts_list(bar_series_total),
  data = ts_list(bar_cluster)
)
```

```ts
{
  schemaVersion: 1,
  type: "bar",
  variables: { v1: "Mode", v2: "Age" },
  panels: [
    {
      total: 100,
      seriesTotals: [
        { label: "Child", total: 20 },
        { label: "Adult", total: 80 }
      ],
      data: [
        {
          label: "Bus",
          series: [
            { label: "Child", count: 6, proportion: 0.3 },
            { label: "Adult", count: 40, proportion: 0.5 }
          ]
        },
        {
          label: "Walk",
          series: [
            { label: "Child", count: 14, proportion: 0.7 },
            { label: "Adult", count: 40, proportion: 0.5 }
          ]
        }
      ]
    }
  ]
}
```

Child is 6 + 14 and Adult is 40 + 40, which is what `seriesTotals` records. The proportions sum to 1 across clusters within a series (Child 0.3 + 0.7), not within a cluster, and only when no zoom has dropped a column.

## Bar, segmented

Still `type: "bar"`, and only when there is no `v2`. The one-way bar stays. `variables.colby` names the stacking factor. `segments` carry the cell `count` (`table`, `xtabs`, or `svytable`, before the column is scaled to 1) and the `proportion` (`p.colby`). The cell count is not `proportion * count`: rows with a missing `colby` stay in the bar `count` and are absent from the segments, so a bar of 5 can have segment counts that sum to 4. The bar’s own `proportion` is still the marginal `phat`. The first `colby` level is the **top** of the stack. Emit factor-level order, not the reversed rows stored on `p.colby`.

```ts
type BarSegmentPanel = {
  s1?: string
  s2?: string
  total: number
  data: Array<{
    label: string
    count: number
    proportion: number
    segments: Array<{ label: string; count: number; proportion: number }>
  }>
}
```

```r
bar_segment <- ts_list(
  label = ts_character(1L),
  count = ts_numeric(1L),
  proportion = ts_numeric(1L)
)

bar_segmented_datum <- ts_list(
  label = ts_character(1L),
  count = ts_numeric(1L),
  proportion = ts_numeric(1L),
  segments = ts_list(bar_segment)
)

bar_segmented_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  total = ts_numeric(1L),
  data = ts_list(bar_segmented_datum)
)
```

```ts
{
  schemaVersion: 1,
  type: "bar",
  variables: { v1: "Day", colby: "Weather" },
  panels: [
    {
      total: 100,
      data: [
        {
          label: "Mon",
          count: 30,
          proportion: 0.3,
          segments: [
            { label: "Rain", count: 12, proportion: 0.4 },
            { label: "Dry", count: 18, proportion: 0.6 }
          ]
        },
        {
          label: "Tue",
          count: 70,
          proportion: 0.7,
          segments: [
            { label: "Rain", count: 14, proportion: 0.2 },
            { label: "Dry", count: 56, proportion: 0.8 }
          ]
        }
      ]
    }
  ]
}
```

## Dot

`type` is `"dot"`. `v2` is groups inside one panel (strips of `df$y`). `s1` is another panel, not another group. Each group’s `points` are the observed x values in increasing order. The client stacks them. R does not send a stack index or a symbol width, and a dot is not tied to the histogram’s bins. One-way data is a single group and omits `label`.

`boxplot` is included when R computed `boxinfo` (on by default, more than five rows, and inference is not the mean). It is `min`, the three quartiles, and `max`. `mean` is included only when the mean indicator is on.

A dot request that this data cannot draw is an error. It is not returned as a histogram.

```ts
type Box = { min: number; q1: number; median: number; q3: number; max: number }

type DotPanel = {
  s1?: string
  s2?: string
  groups: Array<{
    label?: string
    points: Array<{ x: number }>
    boxplot?: Box
    mean?: number
  }>
}
```

```r
box_summary <- ts_list(
  min = ts_numeric(1L),
  q1 = ts_numeric(1L),
  median = ts_numeric(1L),
  q3 = ts_numeric(1L),
  max = ts_numeric(1L)
)

dot_point <- ts_list(
  x = ts_numeric(1L)
)

dot_group <- ts_list(
  label = ts_optional(ts_character(1L)),
  points = ts_list(dot_point),
  boxplot = ts_optional(box_summary),
  mean = ts_optional(ts_numeric(1L))
)

dot_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  groups = ts_list(dot_group)
)
```

```ts
{
  schemaVersion: 1,
  type: "dot",
  variables: { v1: "sw", v2: "g" },
  panels: [
    {
      groups: [
        {
          label: "a",
          points: [{ x: 1 }, { x: 1 }, { x: 2 }],
          boxplot: { min: 1, q1: 1, median: 1, q3: 1.5, max: 2 }
        },
        {
          label: "b",
          points: [{ x: 5 }, { x: 5 }, { x: 5 }, { x: 6 }]
        }
      ]
    }
  ]
}
```

## Histogram

`type` is `"hist"`. One `edges` array per panel. Every group uses those edges, so `counts.length` is `edges.length - 1`. `hist.bins` or dot-point size chooses the edges on the server. The same boxplot rule as the dot applies. A factor inside the panel is `groups`. `s1` is another panel.

Survey and frequency counts are weighted population totals, not sample counts, and need not be integers. The client uses `counts` as bar heights and does not re-bin.

```ts
type HistPanel = {
  s1?: string
  s2?: string
  edges: number[]
  groups: Array<{
    label?: string
    counts: number[]
    boxplot?: Box
    mean?: number
  }>
}
```

```r
hist_group <- ts_list(
  label = ts_optional(ts_character(1L)),
  counts = ts_numeric(0L),
  boxplot = ts_optional(box_summary),
  mean = ts_optional(ts_numeric(1L))
)

hist_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  edges = ts_numeric(0L),
  groups = ts_list(hist_group)
)
```

```ts
{
  schemaVersion: 1,
  type: "hist",
  variables: { v1: "Sepal.Width" },
  panels: [
    {
      edges: [1.876, 2.406, 2.935, 3.465, 3.994, 4.524],
      groups: [
        {
          counts: [11, 46, 68, 21, 4],
          boxplot: { min: 2, q1: 2.8, median: 3, q3: 3.3, max: 4.4 }
        }
      ]
    }
  ]
}
```

## Scatter

`type` is `"scatter"`. One point per plotted row. `id` is `point.order` (the row name after missing `x` or `y` are dropped). Array order is draw order. A join line is not a separate series: connect in ascending `id`, and split by `colby` when lines are coloured by group.

`size` is sent only when it varies by data, and it is the value **before** multiplying by `cex.pt`: frequency `freq / max.freq * 4 + 0.5`, varying survey weights `weight / max.weight * 2 + 0.5`, or the resolved `sizeby` cex. Equal survey weights omit `size`. The survey design is not sent.

`symbol` is sent only for `symbolby` (R pch). `colby` is the raw colour-by value. `highlight` is sent only on highlighted points. Trend and smooth lines are not in this shape.

```ts
type ScatterPanel = {
  s1?: string
  s2?: string
  data: Array<{
    id: number
    x: number
    y: number
    size?: number
    symbol?: number
    colby?: string | number
    highlight?: boolean
  }>
}
```

```r
scatter_point <- ts_list(
  id = ts_integer(1L),
  x = ts_numeric(1L),
  y = ts_numeric(1L),
  size = ts_optional(ts_numeric(1L)),
  symbol = ts_optional(ts_integer(1L)),
  colby = ts_optional(ts_union(ts_character(1L), ts_numeric(1L))),
  highlight = ts_optional(ts_logical(1L))
)

scatter_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  data = ts_list(scatter_point)
)
```

```ts
{
  schemaVersion: 1,
  type: "scatter",
  variables: { v1: "height", v2: "weight" },
  panels: [
    {
      data: [
        { id: 3, x: 160, y: 54 },
        { id: 1, x: 171, y: 66, colby: "B", symbol: 22, highlight: true },
        { id: 8, x: 182, y: 79, colby: "A", symbol: 21 }
      ]
    }
  ]
}
```

## Hex

`type` is `"hex"`. Occupied cells only. No raw `x` or `y`. `x` and `y` are the lattice centre. `count` is the hexbin count (the sum of weights: 1, `freq`, or design weights). `meanX` and `meanY` are the weighted centre of mass. `xBins` is `hex.bins`. `shape` is the hexbin shape. Bounds are that panel’s data range.

`colby` hexes are not in this shape. That path rebins the raw points and does not use this lattice.

```ts
type HexPanel = {
  s1?: string
  s2?: string
  xBounds: [number, number]
  yBounds: [number, number]
  xBins: number
  shape: number
  data: Array<{
    x: number
    y: number
    count: number
    meanX: number
    meanY: number
  }>
}
```

```r
hex_cell <- ts_list(
  x = ts_numeric(1L),
  y = ts_numeric(1L),
  count = ts_numeric(1L),
  meanX = ts_numeric(1L),
  meanY = ts_numeric(1L)
)

hex_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  xBounds = ts_numeric(2L),
  yBounds = ts_numeric(2L),
  xBins = ts_integer(1L),
  shape = ts_numeric(1L),
  data = ts_list(hex_cell)
)
```

```ts
{
  schemaVersion: 1,
  type: "hex",
  variables: { v1: "x", v2: "y" },
  panels: [
    {
      xBounds: [1, 4],
      yBounds: [1, 4],
      xBins: 5,
      shape: 1,
      data: [
        { x: 1, y: 1, count: 2, meanX: 1.1, meanY: 1.05 },
        { x: 2.5, y: 2.5588, count: 1, meanX: 2.4, meanY: 2.8 }
      ]
    }
  ]
}
```

## Grid

`type` is `"grid"`. Non-zero rectangles on the shared axis window, in data coordinates. `n` is `min(250, round(scatter.grid.bins))`. On a facet, `xBounds`, `yBounds`, and `n` are the same on every panel. Counts are unweighted row counts. Frequency and survey weights are not applied. Those plots should be hexes.

```ts
type GridPanel = {
  s1?: string
  s2?: string
  xBounds: [number, number]
  yBounds: [number, number]
  n: number
  data: Array<{
    x0: number
    x1: number
    y0: number
    y1: number
    count: number
  }>
}
```

```r
grid_cell <- ts_list(
  x0 = ts_numeric(1L),
  x1 = ts_numeric(1L),
  y0 = ts_numeric(1L),
  y1 = ts_numeric(1L),
  count = ts_numeric(1L)
)

grid_panel <- ts_list(
  s1 = ts_optional(ts_character(1L)),
  s2 = ts_optional(ts_character(1L)),
  xBounds = ts_numeric(2L),
  yBounds = ts_numeric(2L),
  n = ts_integer(1L),
  data = ts_list(grid_cell)
)
```

```ts
{
  schemaVersion: 1,
  type: "grid",
  variables: { v1: "x", v2: "y" },
  panels: [
    {
      xBounds: [1, 4],
      yBounds: [1, 4],
      n: 4,
      data: [
        { x0: 1, x1: 1.75, y0: 1, y1: 1.75, count: 1 },
        { x0: 1.75, x1: 2.5, y0: 2.5, y1: 3.25, count: 2 }
      ]
    }
  ]
}
```
