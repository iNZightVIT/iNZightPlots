# Plot payload

`as_plot()` turns an `inzplotoutput` into the object a client draws. `schemaVersion` is `1`. Optional fields are omitted. A data-frame column is either on the whole table or absent; a missing value inside a present column stays missing.

Point size, line width, fonts, and whether a bar axis is labelled in counts or percentages stay on the client. Colour scales stay on the `PlotController`. Inference intervals and the missing-value footnote are not in this object.

`type` has to match the variables. R does not substitute another type.

| `type` | Variables | Panel body |
|--------|-----------|------------|
| `bar` | `v1` factor. Optional `v2` factor (side-by-side) or `colby` factor (stacked). Not both. | `data` data frame |
| `dot` | `v1` numeric. Optional `v2` factor (groups inside the panel). | `points` data frame |
| `hist` | `v1` numeric. Optional `v2` factor (groups). | `edges` and `counts` vectors |
| `scatter` | `v1` and `v2` numeric. | `data` data frame |
| `hex` | `v1` and `v2` numeric. | `data` data frame |
| `grid` | `v1` and `v2` numeric. | `data` data frame |

## Plot

| Field | Type | Values |
|-------|------|--------|
| `schemaVersion` | integer | `1` |
| `type` | string | `bar`, `dot`, `hist`, `scatter`, `hex`, `grid` |
| `variables` | object | See below. |
| `layout` | object | See below. Omitted for a single panel. |
| `panels` | array | One object per panel, including a single panel. Flat. Facets add panels; they do not change what `data` means. |

### variables

Names are the plot’s variable names, one field per role, so a column such as `data.colby` can be titled from `variables.colby`. `v1` is always present. A role is included whenever that mapping is set.

| Field | Present when |
|-------|----------------|
| `v1` | Always. Widget `x`. |
| `v2` | A second variable is set. Widget `y`. Inside a panel this is groups or series, not another panel. |
| `s1` | Subset 1 is set (`g1` / `subset1`), even for a single named level. |
| `s2` | Subset 2 is set (`g2` / `subset2`). |
| `colby` | A colour variable is set. Stacking factor on a segmented bar. Names the scatter `colby` column. |
| `symbolby` | A symbol variable is set. Names the scatter `symbol` column. |
| `sizeby` | A size variable is set. Names the scatter `size` column. |

Level requests are not fields. `s1` level `_MULTI` is one panel per level. A named level is that panel only. `s2` level `_ALL` is no second split. `_MULTI` is the `s1` by `s2` matrix. The dropdown’s level list stays on the existing levels call.

There is no `capabilities` field. What an id means, and anything that needs a backend, stays on the `PlotController`.

### layout

Omitted for a single panel, including one named level. Otherwise it only says whether to build a grid or wrap the panels. Direction and where the first panel sits are client parameters.

| Field | Type | Present when |
|-------|------|----------------|
| `matrix` | boolean | Always, when `layout` is present. `true` for an `s1` by `s2` matrix. `false` when `s2` is `_ALL` or absent and `s1` has more than one panel. |

Panel order is `s2` outer and `s1` inner for a matrix, and `s1` order for a wrap. Each panel carries the level it shows.

## Panels

Every panel may have `s1` and `s2` as above. The fields below are the rest.

### Bar

`type` is `bar`. Two panel shapes. A second variable uses one frame either way: `variables.v2` is side by side, and `variables.colby` is stacked.

One variable. `count` is `tab`. `proportion` is `phat`. `total` is `ntotal`. On a survey, use `proportion` as given.

| Field | Type |
|-------|------|
| `total` | number |
| `data.label` | string. Level of `v1`. |
| `data.count` | number |
| `data.proportion` | number |

`data` has one row per `v1` level.

Two variables. `v1` is the cluster. `series` is a level of `v2` or of `colby`. `count` is the cell count. Each row has both shares, and the chart picks one. `prop_series` is the share of that `x` within that series. Side-by-side bars use it. On a survey with `v2` it is `svymean` and must be used as given. `prop_total` is the cell divided by the sum of every cell. A stack uses it. A level that is 5% of each series, with equal series totals, stacks to about 5%, and each segment's `prop_total` is about half of that. Both are taken before zoom and are not renormalised. `data` has one row per cluster and series: each `v1` level, then each series level inside it. The first `colby` level is the top of the stack. A missing `y` or `colby` is dropped, so `total` is the sum of the visible cells.

| Field | Type |
|-------|------|
| `total` | number. Sum of the visible cell counts. |
| `seriesTotals.label` | string. Level of `v2` or `colby`. |
| `seriesTotals.total` | number. Sum of that series across every `v1` level, before zoom drops columns. |
| `data.label` | string. Level of `v1`. |
| `data.series` | string. Level of `v2` or `colby`. |
| `data.count` | number |
| `data.prop_series` | number. Share of this `v1` level within the series. Side-by-side height. |
| `data.prop_total` | number. Share of the whole population. Stacked segment height. |

`seriesTotals` is a data frame. `widths`, `edges`, and `nn` are not sent. Relative widths use `seriesTotals`, not the sum of visible counts.

### Dot

`points` is a data frame of observed `x` in increasing order. The client stacks them. No stack index and no symbol width. One group omits `label`. A dot that this data cannot draw is an error, not a histogram.

| Field | Type | Present when |
|-------|------|----------------|
| `groups[].label` | string | `v2` is set. |
| `groups[].points.x` | number array | Always. |
| `groups[].boxplot` | object | R computed `boxinfo` (on by default, more than five rows, and inference is not the mean). |
| `groups[].mean` | number | The mean indicator is on. |

`boxplot` fields are `min`, `q1`, `median`, `q3`, `max`.

### Histogram

One `edges` array per panel. Every group uses it, so `counts` is one shorter than `edges`. The server chooses the edges. Survey and frequency counts are weighted totals and need not be integers. The client does not re-bin. `boxplot` and `mean` follow the dot rules.

| Field | Type | Present when |
|-------|------|----------------|
| `edges` | number array | Always. |
| `groups[].label` | string | `v2` is set. |
| `groups[].counts` | number array | Always. |
| `groups[].boxplot` | object | Same rule as the dot. |
| `groups[].mean` | number | The mean indicator is on. |

### Scatter

`data` is a data frame, one row per plotted point, in draw order. `id` is `point.order`. A join line connects ascending `id`, split by `colby` when lines are coloured by group. Trend and smooth lines are not included. The survey design is not sent.

| Column | Type | Present when |
|--------|------|----------------|
| `id` | integer | Always. |
| `x` | number | Always. |
| `y` | number | Always. |
| `size` | number | Size varies. Value before multiplying by `cex.pt`: frequency `freq / max.freq * 4 + 0.5`, varying survey weights `weight / max.weight * 2 + 0.5`, or the resolved `sizeby` cex. Equal survey weights omit it. `variables.sizeby` is set whenever a size variable was mapped, including when this column is omitted. |
| `symbol` | integer | `symbolby` is set. R pch. The variable name is `variables.symbolby`. |
| `colby` | factor or number | `variables.colby` is set. A factor stays a factor. Missing values stay missing. |
| `highlight` | logical | At least one point is highlighted. Other rows are `false`. |

An empty panel is a zero-row frame with `id`, `x`, and `y`.

### Hex

Occupied cells only. `x` and `y` are the lattice centre, not the raw points. `count` is the hexbin count (sum of weights: 1, `freq`, or design weights). `meanX` and `meanY` are the weighted centre of mass. Bounds are that panel’s data range. Colour-by hexes are not this shape.

| Field | Type |
|-------|------|
| `xBounds` | `[number, number]` |
| `yBounds` | `[number, number]` |
| `xBins` | integer. `hex.bins`. |
| `shape` | number. Hexbin shape. |
| `data.x`, `data.y` | number |
| `data.count` | number |
| `data.meanX`, `data.meanY` | number |

### Grid

Non-zero rectangles on the shared axis window, in data coordinates, `x` bin then `y` bin. On a facet, `xBounds`, `yBounds`, and `n` are the same on every panel. Counts are unweighted row counts. Frequency and survey weights are not applied; those plots should be hexes.

| Field | Type |
|-------|------|
| `xBounds` | `[number, number]`. Plot `xlim`. |
| `yBounds` | `[number, number]`. Plot `ylim`. |
| `n` | integer. `min(250, round(scatter.grid.bins))`. |
| `data.x0`, `data.x1` | number |
| `data.y0`, `data.y1` | number |
| `data.count` | number |

## Mark tables

Bar `data`, bar `seriesTotals`, dot `points`, scatter `data`, hex `data`, and grid `data` are data frames: one column per field, equal length, in the order above. The histogram is already vectors.

On the wire, Rserve writes a data frame as packed columns. A client that wants one object per mark zips those columns by index. Nulls in a present column stay on the row.
