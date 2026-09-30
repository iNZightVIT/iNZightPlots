/**
 * A mark table is an object whose values are equal-length columns of
 * primitives. Histogram groups are not: they mix objects and arrays.
 */
function isFrame(value: unknown): value is Record<string, unknown[]> {
  if (value === null || typeof value !== "object" || Array.isArray(value)) {
    return false;
  }
  const cols = Object.values(value);
  if (cols.length === 0 || !cols.every(Array.isArray)) return false;
  const n = cols[0].length;
  return cols.every(
    (col) => col.length === n && col.every((item) => item === null || typeof item !== "object"),
  );
}

function rowsFromFrame(frame: Record<string, unknown[]>): Record<string, unknown>[] {
  const keys = Object.keys(frame);
  const n = frame[keys[0]].length;
  const rows: Record<string, unknown>[] = [];
  for (let i = 0; i < n; i++) {
    const row: Record<string, unknown> = {};
    for (const key of keys) row[key] = frame[key][i];
    rows.push(row);
  }
  return rows;
}

/** Walk a plot payload and turn mark tables into arrays of row objects. */
export function toRows(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(toRows);
  if (isFrame(value)) return rowsFromFrame(value);
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([key, item]) => [key, toRows(item)]),
    );
  }
  return value;
}
