import { useCallback, useEffect, useRef, useState } from "react";
import {
  Eye,
  FolderOpen,
  Plus,
  ArrowUpRight,
  ArrowDownToLine,
  Check,
  X,
  ChevronLeft,
  ChevronRight,
  ChevronDown,
  Undo2,
  Search,
  Layers3,
  Activity,
  Circle,
  CheckCircle2,
  AlertCircle,
  Database,
  SlidersHorizontal,
  Keyboard,
  RotateCcw,
  LoaderCircle,
  SkipForward,
  LineChart,
} from "lucide-react";
import { TracePlot } from "./TracePlot";
import { AutoExcludeControl } from "./AutoExcludeControl";
import { groupKey } from "./DiagnosticsWorkspace";
import {
  stageName,
  AUTO_REVIEWER,
  type Epoch,
  type Summary,
  type Trace,
  type Filters,
  type Queue,
  type Status,
} from "./types";

const defaults: Filters = {
  status: "all",
  participant: "",
  run: "",
  search: "",
  stage: "final",
  sort: "natural",
  offset: 0,
};
const statusLabel = {
  unreviewed: "Unreviewed",
  keep: "Keep",
  exclude: "Exclude",
};
const number = (n: number) => n.toLocaleString();
// Kept outside the component so a dismissed export result stays dismissed.
let dismissedExport = "";
function StatusIcon({ status }: { status: Status }) {
  return status === "keep" ? (
    <CheckCircle2 size={15} />
  ) : status === "exclude" ? (
    <X size={15} />
  ) : (
    <Circle size={15} />
  );
}

export function ReviewWorkspace({
  initialProject,
  reviewer: initialReviewer,
  participant = "",
  onSubjects,
  onDiagnostics,
  onClose,
}: {
  initialProject: Summary;
  reviewer: string;
  participant?: string;
  onSubjects: () => void;
  onDiagnostics: (key?: string) => void;
  onClose: () => void;
}) {
  const [project, setProject] = useState<Summary | null>(initialProject);
  const [reviewer, setReviewer] = useState(initialReviewer);
  // Continue where review was left, unless a participant was chosen to review.
  const [resumed] = useState(() => {
    const saved = participant ? null : initialProject.position;
    if (!saved) return null;
    const f = { ...defaults, ...saved.filters };
    if (f.participant && !initialProject.participants.includes(f.participant))
      f.participant = "";
    if (f.run && !initialProject.runs.includes(f.run)) f.run = "";
    if (f.stage !== "final" && !initialProject.stages.includes(f.stage))
      f.stage = "final";
    return { epochId: saved.epochId, filters: f };
  });
  const [filters, setFilters] = useState<Filters>(
    resumed?.filters ?? { ...defaults, participant },
  );
  const [exportOpen, setExportOpen] = useState(false);
  const [, setDismissed] = useState(dismissedExport);
  const [queue, setQueue] = useState<Queue>({ rows: [], total: 0, offset: 0 });
  const [selected, setSelected] = useState<Epoch | null>(null);
  const [trace, setTrace] = useState<(Trace & { epochId: string }) | null>(
    null,
  );
  const [range, setRange] = useState<[number, number]>();
  const [traceLoading, setTraceLoading] = useState(false);
  const [revision, setRevision] = useState(0);
  const [busy, setBusy] = useState("");
  const locked = useRef(false);
  const [error, setError] = useState("");
  const [notice, setNotice] = useState("");
  const [note, setNote] = useState("");
  const [reason, setReason] = useState("Signal artifact");
  const [advance, setAdvance] = useState(true);
  const preferred = useRef<string | null>(resumed?.epochId ?? null);
  const listToken = useRef(0);
  const refresh = () => setRevision((x) => x + 1);
  const act = useCallback(async (label: string, fn: () => Promise<void>) => {
    if (locked.current) return;
    locked.current = true;
    setBusy(label);
    setError("");
    setNotice("");
    try {
      await fn();
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e));
    } finally {
      locked.current = false;
      setBusy("");
    }
  }, []);
  useEffect(() => {
    setProject(initialProject);
    refresh();
  }, [initialProject.sources.length]);
  // Export runs in the background; follow its progress from the app's updates.
  useEffect(() => {
    setProject(
      (p) =>
        p && {
          ...p,
          exporting: initialProject.exporting,
          lastExport: initialProject.lastExport,
        },
    );
  }, [
    JSON.stringify([initialProject.exporting, initialProject.lastExport?.id]),
  ]);
  // Without a saved position, start at the first epoch still to review.
  useEffect(() => {
    if (resumed) return;
    window.eyeris
      .nextUnreviewed(filters, null)
      .then((next) => {
        if (!next) return;
        preferred.current = next.id;
        setFilters((f) => ({ ...f, offset: next.offset }));
      })
      .catch(() => {});
  }, []);
  // Remember the position so a reopened project continues from here.
  useEffect(() => {
    if (!selected) return;
    const timer = setTimeout(
      () =>
        void window.eyeris
          .saveReviewPosition({ epochId: selected.id, filters })
          .catch(() => {}),
      500,
    );
    return () => clearTimeout(timer);
  }, [selected?.id, filters]);
  useEffect(() => {
    if (!project) return;
    const token = ++listToken.current;
    const timer = setTimeout(
      () => {
        window.eyeris
          .list(filters)
          .then((result) => {
            if (token !== listToken.current) return;
            if (!result.rows.length && filters.offset > 0 && result.total > 0) {
              setFilters((f) => ({ ...f, offset: Math.max(0, f.offset - 80) }));
              return;
            }
            setQueue(result);
            const wanted = preferred.current;
            setSelected(
              (current) =>
                (wanted === "__last__" ? result.rows.at(-1) : null) ||
                result.rows.find((e) => e.id === (wanted || current?.id)) ||
                result.rows[0] ||
                null,
            );
            preferred.current = null;
          })
          .catch((e) => {
            if (token === listToken.current) setError(e.message);
          });
      },
      filters.search ? 180 : 0,
    );
    return () => {
      clearTimeout(timer);
      listToken.current++;
    };
  }, [project?.directory, filters, revision]);
  useEffect(() => {
    setRange(undefined);
  }, [selected?.id]);
  useEffect(() => {
    const saved = selected?.reason || "";
    const reasons = [
      "Signal artifact",
      "Excessive missing data",
      "Blink contamination",
      "Baseline issue",
      "Other",
    ];
    const previous = reasons.find(
      (r) => saved === r || saved.startsWith(r + ": "),
    );
    if (selected?.status === "exclude" && previous) {
      setReason(previous);
      setNote(saved.slice(previous.length).replace(/^: /, ""));
    } else setNote(saved);
  }, [selected?.id, selected?.reason, selected?.status]);
  useEffect(() => {
    setTrace(null);
    if (!selected) {
      setTraceLoading(false);
      return;
    }
    let active = true;
    setTraceLoading(true);
    window.eyeris
      .trace(selected.id, filters.stage, range)
      .then((value) => {
        if (active) setTrace({ ...value, epochId: selected.id });
      })
      .catch((e) => {
        if (active) setError(e.message);
      })
      .finally(() => {
        if (active) setTraceLoading(false);
      });
    return () => {
      active = false;
    };
  }, [selected?.id, filters.stage, range, project?.directory]);
  function filter(change: Partial<Filters>) {
    setFilters((f) => ({ ...f, ...change, offset: 0 }));
  }
  async function switchProject(
    method: "createProject" | "openProject" | "demo",
  ) {
    await act(
      method === "demo"
        ? "Preparing real eyeris demo data…"
        : "Opening project…",
      async () => {
        const next = await window.eyeris[method]();
        if (next) {
          setProject(next);
          setSelected(null);
          setFilters(defaults);
          refresh();
        }
      },
    );
  }
  function importFiles() {
    void act("Indexing epochs…", async () => {
      const result = await window.eyeris.importFiles();
      if (!result) return;
      setProject(result.project);
      refresh();
      const errors = result.results
        .filter((r) => r.error)
        .map((r) => `${r.file}: ${r.error}`);
      if (errors.length) setError(errors.join("\n"));
      const count = result.results.reduce((n, r) => n + (r.count || 0), 0);
      const auto = result.results.reduce(
        (n, r) => n + (r.autoExcluded || 0),
        0,
      );
      setNotice(
        `${number(count)} epochs imported.${auto ? ` ${number(auto)} excluded automatically for missing data.` : ""}${result.results.some((r) => r.duplicate) ? " Already-imported sources were skipped." : ""}`,
      );
    });
  }
  const position = queue.rows.findIndex((e) => e.id === selected?.id);
  function navigate(direction: number) {
    if (locked.current) return;
    const next = queue.rows[position + direction];
    if (next) setSelected(next);
    else if (direction > 0 && filters.offset + queue.rows.length < queue.total)
      setFilters((f) => ({ ...f, offset: f.offset + 80 }));
    else if (direction < 0 && filters.offset > 0) {
      preferred.current = "__last__";
      setFilters((f) => ({ ...f, offset: Math.max(0, f.offset - 80) }));
    }
  }
  async function decide(status: Status) {
    if (
      !selected ||
      !trace ||
      traceLoading ||
      trace.epochId !== selected.id ||
      (filters.stage !== "final" && filters.stage !== trace.stage)
    )
      return;
    await act("Saving decision…", async () => {
      const result = await window.eyeris.decide({
        id: selected.id,
        status,
        stage: trace.stage,
        reviewer,
        reason:
          status === "exclude"
            ? [reason, note].filter(Boolean).join(": ")
            : note,
      });
      setProject(result.project);
      const leavesQueue = filters.status !== "all" && status !== filters.status;
      preferred.current = selected.id;
      if (advance || leavesQueue) {
        // Filtered queues shrink when an epoch changes status, including at a
        // page boundary. Find the successor in the post-decision order.
        const nextOffset = filters.offset + position + (leavesQueue ? 0 : 1);
        const next = await window.eyeris.list({
          ...filters,
          offset: nextOffset,
        });
        if (next.rows[0]) {
          preferred.current = next.rows[0].id;
          setFilters((f) => ({
            ...f,
            offset: Math.floor(nextOffset / 80) * 80,
          }));
        }
      }
      refresh();
      setNotice("Decision saved");
    });
  }
  function undo() {
    void act("Restoring previous decision…", async () => {
      const result = await window.eyeris.undo();
      setProject(result.project);
      if (result.epoch) {
        preferred.current = result.epoch.id;
        setSelected(result.epoch);
        setNotice("Previous decision restored");
      }
      refresh();
    });
  }
  function exportData() {
    void act("Starting export…", async () => {
      const result = await window.eyeris.exportData();
      if (result) {
        setProject(result);
        setExportOpen(false);
      }
    });
  }
  // Jump to the next epoch without a decision, in queue order.
  function nextUnreviewed(
    scope: Filters = filters,
    from: string | null = selected?.id ?? null,
  ) {
    void act("Finding the next unreviewed epoch…", async () => {
      const view = {
        ...scope,
        status: scope.status === "unreviewed" ? "unreviewed" : "all",
      } as Filters;
      const next = await window.eyeris.nextUnreviewed(view, from);
      if (!next) {
        setFilters(view);
        setNotice("Every epoch in this view has been reviewed.");
        return;
      }
      preferred.current = next.id;
      setFilters({ ...view, offset: next.offset });
      refresh();
    });
  }
  useEffect(() => {
    const key = (event: KeyboardEvent) => {
      if (
        event.target instanceof HTMLElement &&
        (["INPUT", "SELECT", "TEXTAREA"].includes(event.target.tagName) ||
          event.target.isContentEditable)
      )
        return;
      if (event.repeat || locked.current || !project) return;
      if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() !== "z")
        return;
      if (event.key.toLowerCase() === "k") {
        event.preventDefault();
        void decide("keep");
      }
      if (event.key.toLowerCase() === "x") {
        event.preventDefault();
        void decide("exclude");
      }
      if (event.key.toLowerCase() === "n") {
        event.preventDefault();
        nextUnreviewed();
      }
      if (event.key === "ArrowDown" || event.key === "ArrowRight") {
        event.preventDefault();
        navigate(1);
      }
      if (event.key === "ArrowUp" || event.key === "ArrowLeft") {
        event.preventDefault();
        navigate(-1);
      }
      if (
        (event.key.toLowerCase() === "u" ||
          ((event.metaKey || event.ctrlKey) && event.key === "z")) &&
        project.canUndo
      ) {
        event.preventDefault();
        undo();
      }
    };
    window.addEventListener("keydown", key);
    return () => window.removeEventListener("keydown", key);
  });
  const reviewed = project ? project.counts.keep + project.counts.exclude : 0;
  const pct = project?.counts.total
    ? Math.round((reviewed / project.counts.total) * 100)
    : 0;

  return (
    <div className="app-shell">
      <aside className="sidebar">
        <div className="brand">
          <img src="./sticker.png" alt="eyeris" />
          <strong>eyeris</strong>
        </div>
        <div className="sidebar-project">{project?.name}</div>
        <button className="nav-item" onClick={onSubjects}>
          <Database size={17} /> Subjects & processing
        </button>
        <div className="nav-item active">
          <Layers3 size={17} /> Epoch review{" "}
          <span className="nav-count">{project?.counts.total}</span>
        </div>
        <button className="nav-item" onClick={() => onDiagnostics()}>
          <LineChart size={17} /> Diagnostics
        </button>
        <button
          className="import-sidebar"
          disabled={!!busy}
          onClick={importFiles}
        >
          <Plus size={16} /> Import processed RDS
        </button>
        <div className="reviewer-field">
          <label htmlFor="reviewer">Reviewer</label>
          <input
            id="reviewer"
            value={reviewer}
            onChange={(e) => setReviewer(e.target.value)}
          />
        </div>
        <div className="sidebar-bottom">
          <button onClick={onClose}>
            <FolderOpen size={15} /> Projects
          </button>
        </div>
      </aside>
      <main>
        <header className="topbar">
          <div className="breadcrumbs">
            Workspace <ChevronRight size={13} /> <span>Epoch review</span>
          </div>
          <span className="save-state">
            {busy ? (
              <LoaderCircle size={14} className="spin" />
            ) : (
              <CheckCircle2 size={14} />
            )}{" "}
            {busy || "Changes saved locally"}
          </span>
        </header>
        <div className="content">
          <div className="page-heading">
            <div>
              <div className="eyebrow">EPOCH REVIEW</div>
              <h1>Epoch review</h1>
              <p>Inspect epochs and record keep/exclude decisions.</p>
            </div>
            {project && (
              <div className="heading-actions">
                <button
                  className="button"
                  disabled={!!busy || !project.counts.unreviewed}
                  onClick={() => nextUnreviewed()}
                >
                  <SkipForward size={16} /> Next unreviewed <kbd>N</kbd>
                </button>
                <button
                  className="button"
                  disabled={!!busy || !project.canUndo}
                  onClick={undo}
                >
                  <Undo2 size={16} /> Undo <kbd>U</kbd>
                </button>
                <button
                  className="button primary"
                  disabled={
                    !!busy || !project.counts.total || !!project.exporting
                  }
                  onClick={() => setExportOpen(!exportOpen)}
                >
                  <ArrowDownToLine size={16} /> Export review
                </button>
              </div>
            )}
          </div>
          {error && (
            <div className="message error" role="alert">
              <AlertCircle size={17} />
              <span>{error}</span>
              <button aria-label="Dismiss error" onClick={() => setError("")}>
                <X size={16} />
              </button>
            </div>
          )}
          {notice && (
            <div className="message notice" role="status">
              <CheckCircle2 size={16} />
              <span>{notice}</span>
              <button
                aria-label="Dismiss notification"
                onClick={() => setNotice("")}
              >
                <X size={16} />
              </button>
            </div>
          )}
          {!project || !project.counts.total ? (
            <section className="welcome">
              <h2>No epochs in this project</h2>
              <p>
                Process a recording with epoching enabled, or import an epoched
                eyeris object.
              </p>
              <div className="welcome-actions">
                <button className="button primary" onClick={onSubjects}>
                  Go to subjects
                </button>
                <button className="button" onClick={importFiles}>
                  Import processed RDS
                </button>
              </div>
            </section>
          ) : (
            <>
              {project.exporting && (
                <section className="export-progress" role="status">
                  <div>
                    <strong>
                      <LoaderCircle size={15} className="spin" /> Exporting
                      every subject
                    </strong>
                    <span>
                      {project.exporting.completed} of{" "}
                      {project.exporting.total || "…"} sources
                    </span>
                  </div>
                  <div
                    className="progress-track"
                    role="progressbar"
                    aria-label="Export progress"
                    aria-valuemin={0}
                    aria-valuemax={project.exporting.total || 1}
                    aria-valuenow={project.exporting.completed}
                  >
                    <span
                      style={{
                        width: `${(100 * project.exporting.completed) / (project.exporting.total || 1)}%`,
                      }}
                    />
                  </div>
                  <small>{project.exporting.current}</small>
                </section>
              )}
              {!project.exporting &&
                project.lastExport &&
                project.lastExport.id !== dismissedExport && (
                  <div
                    className={`message ${project.lastExport.error ? "error" : "notice"}`}
                    role="status"
                  >
                    {project.lastExport.error ? (
                      <AlertCircle size={17} />
                    ) : (
                      <CheckCircle2 size={16} />
                    )}
                    <div>
                      {project.lastExport.error ? (
                        <>Export failed: {project.lastExport.error}</>
                      ) : (
                        <>
                          Exported {number(project.lastExport.counts!.keep)}{" "}
                          kept, {number(project.lastExport.counts!.exclude)}{" "}
                          excluded and{" "}
                          {number(project.lastExport.counts!.unreviewed)}{" "}
                          unreviewed epochs to {project.lastExport.directory}
                          <button
                            className="link-button"
                            onClick={() =>
                              void act("Opening export…", () =>
                                window.eyeris.showProjectFiles("export"),
                              )
                            }
                          >
                            Show exported files
                          </button>
                        </>
                      )}
                    </div>
                    <button
                      aria-label="Dismiss export result"
                      onClick={() => {
                        dismissedExport = project.lastExport!.id;
                        setDismissed(dismissedExport);
                      }}
                    >
                      <X size={16} />
                    </button>
                  </div>
                )}
              {exportOpen && !project.exporting && (
                <section className="export-panel">
                  <div>
                    <h2>Export every subject</h2>
                    <button
                      aria-label="Close export"
                      onClick={() => setExportOpen(false)}
                    >
                      <X size={16} />
                    </button>
                  </div>
                  <p>
                    {number(project.counts.total)} epochs from{" "}
                    {number(project.progress.length)} participant
                    {project.progress.length === 1 ? "" : "s"}:{" "}
                    {number(project.counts.keep)} kept,{" "}
                    {number(project.counts.exclude)} excluded and{" "}
                    {number(project.counts.unreviewed)} not yet reviewed.
                  </p>
                  {!!project.counts.unreviewed && (
                    <p className="export-warning">
                      Unreviewed epochs are exported separately and are never
                      treated as kept. Decisions are saved as you go, so you can
                      finish reviewing later and export again; every export is a
                      new folder.
                    </p>
                  )}
                  <p>
                    The export holds retained/, excluded/ and unreviewed/
                    folders by subject and session, with one table per run in
                    CSV and RDS, plus decisions.csv, summary.csv and
                    manifest.json.
                  </p>
                  <button
                    className="button primary"
                    disabled={!!busy}
                    onClick={exportData}
                  >
                    <ArrowDownToLine size={16} /> Choose a folder and export
                  </button>
                </section>
              )}
              <div className="stats-grid">
                <div className="stat">
                  <span>
                    Review progress <Activity size={15} />
                  </span>
                  <strong>
                    {pct}
                    <em>%</em>
                    <small>
                      {number(reviewed)} / {number(project.counts.total)}
                    </small>
                  </strong>
                  <div className="progress-track">
                    <span style={{ width: `${pct}%` }} />
                  </div>
                </div>
                <div className="stat">
                  <span>
                    Kept <CheckCircle2 size={15} className="green" />
                  </span>
                  <strong>{number(project.counts.keep)}</strong>
                  <small>Ready for analysis</small>
                </div>
                <div className="stat">
                  <span>
                    Excluded <X size={15} className="red" />
                  </span>
                  <strong>{number(project.counts.exclude)}</strong>
                  <small>Separated on export</small>
                </div>
                <div className="stat">
                  <span>
                    To review <Circle size={15} />
                  </span>
                  <strong>{number(project.counts.unreviewed)}</strong>
                  <small>Never automatically kept</small>
                </div>
              </div>
              <details className="auto-exclude-panel">
                <summary>
                  Automatic exclusion ·{" "}
                  {project.autoExclude.enabled
                    ? `more than ${project.autoExclude.threshold}% missing in ${project.autoExclude.stage === "final" ? "the final stage" : project.autoExclude.stage} · ${number(project.autoExcluded)} excluded`
                    : "off"}
                </summary>
                <AutoExcludeControl
                  project={project}
                  disabled={!!busy}
                  onProject={(next) => {
                    setProject(next);
                    refresh();
                  }}
                />
              </details>
              {project.progress.length > 1 && (
                <details className="subject-progress">
                  <summary>
                    Progress by subject ·{" "}
                    {project.progress.filter((p) => !p.unreviewed).length} of{" "}
                    {project.progress.length} complete
                  </summary>
                  <table>
                    <thead>
                      <tr>
                        <th>Participant</th>
                        <th>Reviewed</th>
                        <th>Kept</th>
                        <th>Excluded</th>
                        <th>To review</th>
                        <th />
                      </tr>
                    </thead>
                    <tbody>
                      {project.progress.map((p) => (
                        <tr key={p.participant}>
                          <td>sub-{p.participant}</td>
                          <td>
                            <span className="progress-track">
                              <span
                                style={{
                                  width: `${(100 * (p.total - p.unreviewed)) / p.total}%`,
                                }}
                              />
                            </span>
                            {number(p.total - p.unreviewed)} / {number(p.total)}
                          </td>
                          <td>{number(p.keep)}</td>
                          <td>{number(p.exclude)}</td>
                          <td>{number(p.unreviewed)}</td>
                          <td>
                            <button
                              disabled={!!busy}
                              onClick={() =>
                                nextUnreviewed(
                                  {
                                    ...defaults,
                                    stage: filters.stage,
                                    participant: p.participant,
                                  },
                                  null,
                                )
                              }
                            >
                              {p.unreviewed ? "Continue" : "View"}
                            </button>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </details>
              )}
              <div className="filter-bar">
                <label className="search">
                  <Search size={16} />
                  <input
                    aria-label="Search epochs"
                    placeholder="Search events, trials, participants…"
                    value={filters.search}
                    onChange={(e) => filter({ search: e.target.value })}
                  />
                </label>
                <label className="select-field">
                  <SlidersHorizontal size={15} />
                  <select
                    aria-label="Participant filter"
                    value={filters.participant}
                    onChange={(e) => filter({ participant: e.target.value })}
                  >
                    <option value="">All participants</option>
                    {project.participants.map((p) => (
                      <option key={p} value={p}>
                        sub-{p}
                      </option>
                    ))}
                  </select>
                </label>
                {project.runs.length > 1 && (
                  <label className="select-field">
                    <select
                      aria-label="Run filter"
                      value={filters.run}
                      onChange={(e) => filter({ run: e.target.value })}
                    >
                      <option value="">All runs</option>
                      {project.runs.map((r) => (
                        <option key={r} value={r}>
                          Run {r}
                        </option>
                      ))}
                    </select>
                  </label>
                )}
                <label className="select-field">
                  <select
                    aria-label="Sort epochs"
                    value={filters.sort}
                    onChange={(e) => filter({ sort: e.target.value })}
                  >
                    <option value="natural">Acquisition order</option>
                    <option value="missing">Most missing · final stage</option>
                  </select>
                </label>
              </div>
              <div className="review-workspace">
                <section className="queue-panel">
                  <div className="panel-title">
                    <h2>Trial queue</h2>
                    <span>{number(queue.total)} epochs</span>
                  </div>
                  <div
                    className="queue-tabs"
                    role="group"
                    aria-label="Review status filter"
                  >
                    {(["all", "unreviewed", "keep", "exclude"] as const).map(
                      (s) => (
                        <button
                          key={s}
                          aria-pressed={filters.status === s}
                          className={filters.status === s ? "selected" : ""}
                          onClick={() => filter({ status: s })}
                        >
                          {s === "all"
                            ? "All"
                            : s === "unreviewed"
                              ? "To review"
                              : s === "keep"
                                ? "Kept"
                                : "Excluded"}
                        </button>
                      ),
                    )}
                  </div>
                  <div className="epoch-list">
                    {queue.rows.map((e) => (
                      <button
                        className={`epoch-row ${selected?.id === e.id ? "selected" : ""}`}
                        key={e.id}
                        aria-pressed={selected?.id === e.id}
                        disabled={!!busy}
                        onClick={() => setSelected(e)}
                      >
                        <span className={`epoch-status ${e.status}`}>
                          <StatusIcon status={e.status} />
                        </span>
                        <span className="epoch-info">
                          <strong>
                            Trial {e.trial}
                            <small>sub-{e.participant}</small>
                          </strong>
                          <span>{e.event}</span>
                          <small>
                            {e.run ? `Run ${e.run}` : e.block} ·{" "}
                            {e.label.replace("epoch_", "")}
                            {e.eye !== "main" ? ` · ${e.eye}` : ""}
                            {e.reviewer === AUTO_REVIEWER && " · auto"}
                          </small>
                        </span>
                        <ChevronRight size={14} />
                      </button>
                    ))}
                    {!queue.rows.length && (
                      <div className="empty-queue">
                        <CheckCircle2 size={25} />
                        <strong>No matching epochs</strong>
                        <span>Try another filter or participant.</span>
                      </div>
                    )}
                  </div>
                  <div className="queue-footer">
                    <span>
                      {queue.total
                        ? `${filters.offset + 1}–${Math.min(filters.offset + 80, queue.total)}`
                        : "0"}{" "}
                      of {number(queue.total)}
                    </span>
                    <button
                      aria-label="Previous page"
                      disabled={!!busy || filters.offset === 0}
                      onClick={() =>
                        setFilters((f) => ({
                          ...f,
                          offset: Math.max(0, f.offset - 80),
                        }))
                      }
                    >
                      <ChevronLeft size={16} />
                    </button>
                    <button
                      aria-label="Next page"
                      disabled={!!busy || filters.offset + 80 >= queue.total}
                      onClick={() =>
                        setFilters((f) => ({ ...f, offset: f.offset + 80 }))
                      }
                    >
                      <ChevronRight size={16} />
                    </button>
                  </div>
                </section>
                <div className="detail-column">
                  <section className="signal-panel">
                    <div className="signal-heading">
                      <div>
                        <div className="eyebrow">
                          {selected
                            ? `SUB-${selected.participant} / ${selected.run ? `RUN ${selected.run}` : selected.block}`
                            : "SIGNAL INSPECTOR"}
                        </div>
                        <h2>
                          {selected
                            ? `Trial ${selected.trial}`
                            : "Select an epoch"}{" "}
                          {selected && (
                            <span className={`status-pill ${selected.status}`}>
                              <StatusIcon status={selected.status} />
                              {statusLabel[selected.status]}
                            </span>
                          )}
                        </h2>
                        <p>
                          {selected
                            ? [
                                selected.event,
                                `sub-${selected.participant}`,
                                selected.session && `ses-${selected.session}`,
                                selected.task && `task-${selected.task}`,
                                selected.run && `Run ${selected.run}`,
                              ]
                                .filter(Boolean)
                                .join(" · ")
                            : "Choose a trial from the queue to inspect its signal."}
                        </p>
                      </div>
                      <div className="navigation">
                        <button
                          aria-label="Previous epoch"
                          disabled={
                            !!busy ||
                            !selected ||
                            (position === 0 && filters.offset === 0)
                          }
                          onClick={() => navigate(-1)}
                        >
                          <ChevronLeft size={18} />
                        </button>
                        <button
                          aria-label="Next epoch"
                          disabled={
                            !!busy ||
                            !selected ||
                            filters.offset + position + 1 >= queue.total
                          }
                          onClick={() => navigate(1)}
                        >
                          <ChevronRight size={18} />
                        </button>
                      </div>
                    </div>
                    <div className="stage-toolbar">
                      <span>
                        <Layers3 size={15} /> Preprocessing stage
                      </span>
                      <label>
                        <select
                          aria-label="Preprocessing stage"
                          value={filters.stage}
                          onChange={(e) => {
                            setRange(undefined);
                            filter({ stage: e.target.value });
                          }}
                        >
                          <option value="final">Final available stage</option>
                          {project.stages.map((s) => (
                            <option key={s} value={s}>
                              {stageName(s)} · {s.replace("pupil_", "")}
                            </option>
                          ))}
                        </select>
                        <ChevronDown size={14} />
                      </label>
                      <button
                        className="run-average"
                        disabled={!selected}
                        onClick={() =>
                          selected && onDiagnostics(groupKey(selected))
                        }
                      >
                        <LineChart size={14} /> Run average
                      </button>
                    </div>
                    <div className="plot-heading">
                      <span>
                        <i />
                        {trace ? stageName(trace.stage) : "Pupil signal"}
                      </span>
                      <button
                        disabled={!range}
                        onClick={() => setRange(undefined)}
                      >
                        <RotateCcw size={13} /> Reset zoom
                      </button>
                    </div>
                    {trace && !traceLoading ? (
                      <TracePlot
                        trace={trace}
                        range={range}
                        onRange={setRange}
                      />
                    ) : (
                      <div className="plot-placeholder">
                        {traceLoading ? (
                          <>
                            <LoaderCircle className="spin" size={24} />
                            <span>Loading signal…</span>
                          </>
                        ) : (
                          <>
                            <Activity size={30} />
                            <span>No signal selected</span>
                          </>
                        )}
                      </div>
                    )}
                    <div className="signal-metadata">
                      <span>
                        <strong>
                          {number(
                            trace?.samples || selected?.meta.samples || 0,
                          )}
                        </strong>{" "}
                        samples
                      </span>
                      <span>
                        <strong>
                          {selected?.meta.duration.toFixed(2) || "—"} s
                        </strong>{" "}
                        duration
                      </span>
                      <span>
                        <strong>
                          {trace ? `${(trace.missing * 100).toFixed(1)}%` : "—"}
                        </strong>{" "}
                        missing at this stage
                      </span>
                      <span>
                        {trace && trace.displayed < trace.samples
                          ? `${number(trace.displayed)} display points`
                          : "All stored samples"}
                      </span>
                    </div>
                  </section>
                  <section className="decision-panel">
                    <div className="decision-heading">
                      <div>
                        <h2>Your decision</h2>
                        <p>Applies to this epoch across all stored stages.</p>
                      </div>
                      <label className="auto-advance">
                        <input
                          type="checkbox"
                          checked={advance}
                          onChange={(e) => setAdvance(e.target.checked)}
                        />{" "}
                        Auto-advance
                      </label>
                    </div>
                    <div className="decision-fields">
                      <label>
                        Exclusion reason
                        <select
                          aria-label="Exclusion reason"
                          value={reason}
                          onChange={(e) => setReason(e.target.value)}
                        >
                          <option>Signal artifact</option>
                          <option>Excessive missing data</option>
                          <option>Blink contamination</option>
                          <option>Baseline issue</option>
                          <option>Other</option>
                        </select>
                      </label>
                      <label>
                        Review note <span>optional</span>
                        <input
                          aria-label="Review note"
                          placeholder="Add context to your decision…"
                          maxLength={1800}
                          value={note}
                          onChange={(e) => setNote(e.target.value)}
                        />
                      </label>
                    </div>
                    <div className="decision-actions">
                      <button
                        className="button keep-button"
                        disabled={
                          !!busy || !trace || traceLoading || !reviewer.trim()
                        }
                        onClick={() => void decide("keep")}
                      >
                        <Check size={18} /> Keep epoch <kbd>K</kbd>
                      </button>
                      <button
                        className="button exclude-button"
                        disabled={
                          !!busy || !trace || traceLoading || !reviewer.trim()
                        }
                        onClick={() => void decide("exclude")}
                      >
                        <X size={18} /> Exclude epoch <kbd>X</kbd>
                      </button>
                      <button
                        className="reset-decision"
                        disabled={
                          !!busy ||
                          !trace ||
                          traceLoading ||
                          selected?.status === "unreviewed"
                        }
                        onClick={() => void decide("unreviewed")}
                      >
                        Mark unreviewed
                      </button>
                    </div>
                  </section>
                  <div className="review-footnote">
                    <Keyboard size={15} />
                    <span>
                      <kbd>↑</kbd> <kbd>↓</kbd> navigate <kbd>K</kbd> keep{" "}
                      <kbd>X</kbd> exclude <kbd>U</kbd> undo
                    </span>
                    <span>Original data preserved</span>
                  </div>
                </div>
              </div>
            </>
          )}
        </div>
      </main>
    </div>
  );
}
