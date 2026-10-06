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
} from "lucide-react";
import { TracePlot } from "./TracePlot";
import {
  stageName,
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
  onSubjects,
  onClose,
}: {
  initialProject: Summary;
  reviewer: string;
  onSubjects: () => void;
  onClose: () => void;
}) {
  const [project, setProject] = useState<Summary | null>(initialProject);
  const [reviewer, setReviewer] = useState(initialReviewer);
  const [filters, setFilters] = useState<Filters>(defaults);
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
  const preferred = useRef<string | null>(null);
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
      setNotice(
        `${number(count)} epochs imported.${result.results.some((r) => r.duplicate) ? " Already-imported sources were skipped." : ""}`,
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
    void act("Exporting full-resolution epoch tables…", async () => {
      const result = await window.eyeris.exportData();
      if (result)
        setNotice(
          `Exported ${number(result.counts.keep)} kept, ${number(result.counts.exclude)} excluded, and ${number(result.counts.unreviewed)} unreviewed epochs to ${result.directory}`,
        );
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
                  disabled={!!busy || !project.canUndo}
                  onClick={undo}
                >
                  <Undo2 size={16} /> Undo <kbd>U</kbd>
                </button>
                <button
                  className="button primary"
                  disabled={!!busy || !project.counts.total}
                  onClick={exportData}
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
                            {e.block.replace("block_", "Run ")} ·{" "}
                            {e.label.replace("epoch_", "")}
                            {e.eye !== "main" ? ` · ${e.eye}` : ""}
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
                            ? `SUB-${selected.participant} / ${selected.block.replace("block_", "RUN ")}`
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
                          {selected?.event ||
                            "Choose a trial from the queue to inspect its signal."}
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
