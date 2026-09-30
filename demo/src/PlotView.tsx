import { toRows } from "./rows";

/**
 * Host for one plot payload. A later chart will keep a d3 instance on an
 * empty element and call update() on new data, so enter/exit joins are not
 * React children. This demo prints the payload as received, and the same
 * payload after mark tables are zipped into rows.
 */
export function PlotView({ structureJson }: { structureJson: string }) {
  let raw = structureJson;
  let rows: string | null = null;
  try {
    const parsed: unknown = JSON.parse(structureJson);
    raw = JSON.stringify(parsed, null, 2);
    rows = JSON.stringify(toRows(parsed), null, 2);
  } catch {
    rows = null;
  }
  return (
    <div className="plot-views">
      <section>
        <h3>As received</h3>
        <pre className="plot-json">{raw}</pre>
      </section>
      {rows ? (
        <section>
          <h3>Rows</h3>
          <pre className="plot-json">{rows}</pre>
        </section>
      ) : null}
    </div>
  );
}
