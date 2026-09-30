import { useEffect, useState } from "react";
import RserveClient from "rserve-ts";
import schema from "./plots.rserve";
import { PlotView } from "./PlotView";

const RSERVE_HOST = "http://127.0.0.1:6312";

type Example = {
  id: string;
  title: string;
  structureJson: string;
};

export function App() {
  const [examples, setExamples] = useState<Example[]>([]);
  const [id, setId] = useState<string | null>(null);
  const [status, setStatus] = useState("Connecting to Rserve…");

  useEffect(() => {
    let cancel = false;
    (async () => {
      try {
        const client = await RserveClient.create({ host: RSERVE_HOST });
        const app = await client.ocap(schema);
        const result = await app.plots();
        if (cancel) return;
        const rows = result.map((row) => ({
          id: row.id,
          title: row.title,
          structureJson: row.structureJson,
        }));
        setExamples(rows);
        setId(rows[0]?.id ?? null);
        setStatus("");
      } catch (err) {
        if (cancel) return;
        const message = err instanceof Error ? err.message : String(err);
        setStatus(
          `Could not reach Rserve at ${RSERVE_HOST}. Run \`Rscript src/plots.rserve.R\` from demo/. ${message}`,
        );
      }
    })();
    return () => {
      cancel = true;
    };
  }, []);

  const selected = examples.find((item) => item.id === id) ?? null;

  return (
    <div className="app">
      <aside>
        <h1>Plot payloads</h1>
        <p className="note">Census at school, 5000</p>
        <ul>
          {examples.map((item) => (
            <li key={item.id}>
              <button
                type="button"
                aria-current={item.id === selected?.id ? "true" : undefined}
                onClick={() => setId(item.id)}
              >
                {item.title}
              </button>
            </li>
          ))}
        </ul>
      </aside>
      <main>
        {status ? <p className="status">{status}</p> : null}
        {selected ? (
          <>
            <h2>{selected.title}</h2>
            <PlotView structureJson={selected.structureJson} />
          </>
        ) : null}
      </main>
    </div>
  );
}
