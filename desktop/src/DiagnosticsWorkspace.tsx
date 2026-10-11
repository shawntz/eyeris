import { useEffect, useState } from "react";
import {
  Activity,
  AlertCircle,
  ChevronLeft,
  ChevronRight,
  Database,
  FolderOpen,
  Layers3,
  LineChart,
  LoaderCircle,
  X,
} from "lucide-react";
import { AveragePlot } from "./AveragePlot";
import {
  stageName,
  type Average,
  type Behavior,
  type DiagnosticGroup,
  type DiagnosticsActivity,
  type DiagnosticsProgress,
  type Include,
  type Split,
  type SplitRequest,
  type Summary,
} from "./types";

// Distinct, readable colors for up to eight groups.
const palette = [
  "#820000",
  "#1f5f8b",
  "#2f7d4f",
  "#b36b00",
  "#6b3fa0",
  "#00838f",
  "#8d6e63",
  "#4d4d4d",
];

const number = (n: number) => n.toLocaleString();
// What a long average or link is doing, with a bar when its size is known.
function ProgressLine({
  progress,
  compact = false,
}: {
  progress: DiagnosticsProgress | null;
  compact?: boolean;
}) {
  if (!progress) return null;
  const { kind, done, total, epochs } = progress;
  const text =
    kind === "link"
      ? total
        ? `Reading behavioral files: ${number(done)} of ${number(total)}`
        : "Finding behavioral files…"
      : kind === "fields"
        ? `Reading epoch fields: ${number(done)} of ${number(total)} runs`
        : !total
          ? "Selecting epochs…"
          : compact
            ? `${number(done)} of ${number(total)} runs read`
            : `Averaging ${number(epochs ?? 0)} epochs: ${number(done)} of ${number(total)} runs read`;
  return (
    <span className={`diagnostics-progress${compact ? " compact" : ""}`}>
      <span>{text}</span>
      {!!total && !compact && (
        <span
          className="progress-track"
          role="progressbar"
          aria-label={
            kind === "link" ? "Linking progress" : "Averaging progress"
          }
          aria-valuemin={0}
          aria-valuemax={total}
          aria-valuenow={done}
        >
          <span style={{ width: `${(100 * done) / total}%` }} />
        </span>
      )}
    </span>
  );
}
export const groupKey = (e: {
  source_id: string;
  label: string;
  eye: string;
  block: string;
}) => [e.source_id, e.label, e.eye, e.block].join("|");
function groupName(g: DiagnosticGroup, groups: DiagnosticGroup[]) {
  const name = [
    `sub-${g.participant}`,
    g.session && `ses-${g.session}`,
    g.task && `task-${g.task}`,
    g.run ? `Run ${g.run}` : g.block,
    g.label.replace(/^epoch_/, ""),
    g.eye !== "main" && `${g.eye} eye`,
  ]
    .filter(Boolean)
    .join(" · ");
  // A run indexed from two sources is told apart by its source file.
  const twin = groups.some(
    (o) =>
      o !== g &&
      o.participant === g.participant &&
      o.session === g.session &&
      o.task === g.task &&
      o.run === g.run &&
      o.label === g.label &&
      o.eye === g.eye,
  );
  return twin ? `${name} · ${g.source_name}` : name;
}

// The average trace of each run's epochs, over every epoch's trace, to check a
// run's overall response and spot runs or epochs that look unlike the rest.
export function DiagnosticsWorkspace({
  project,
  initialKey,
  onSubjects,
  onReview,
  onClose,
}: {
  project: Summary;
  initialKey?: string;
  onSubjects: () => void;
  onReview: (participant?: string) => void;
  onClose: () => void;
}) {
  const [groups, setGroups] = useState<DiagnosticGroup[]>([]);
  const [key, setKey] = useState(initialKey ?? "");
  const [stage, setStage] = useState("final");
  const [include, setInclude] = useState<Include>("included");
  const [showTraces, setShowTraces] = useState(true);
  const [average, setAverage] = useState<Average | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState("");
  // Grouped averages: split by an epoch field or a behavioral column, over a
  // run, a subject's runs, or every subject.
  const [splitBy, setSplitBy] = useState("");
  const [scope, setScope] = useState<SplitRequest["scope"]>("run");
  const [joinEpoch, setJoinEpoch] = useState("");
  const [joinBehavior, setJoinBehavior] = useState("");
  const [fields, setFields] = useState<string[]>([]);
  const [behavior, setBehavior] = useState<Behavior | null>(null);
  const [split, setSplit] = useState<Split | null>(null);
  const [linking, setLinking] = useState(false);
  const [activity, setActivity] = useState<DiagnosticsActivity>({});
  // Reading epoch fields comes first when an average needs them.
  const progress = activity.fields ?? activity.average ?? null;
  const grouped = !!splitBy || scope !== "run";
  const [from, column] = splitBy
    ? (splitBy.split(/:(.*)/s) as ["epoch" | "behavior", string])
    : [null, ""];
  const needsJoin = from === "behavior" && !(joinEpoch && joinBehavior);
  useEffect(() => {
    window.eyeris
      .diagnosticGroups()
      .then((list) => {
        setGroups(list);
        setKey((k) =>
          list.some((g) => g.key === k) ? k : (list[0]?.key ?? ""),
        );
      })
      .catch((e) => setError(e.message));
    window.eyeris
      .epochFields()
      .then(setFields)
      .catch((e) => setError(e.message));
    window.eyeris
      .behavior()
      .then(setBehavior)
      .catch((e) => setError(e.message));
  }, [project.sources.length, project.counts.total]);
  // Suggest the epoch field and behavioral column that share a name, such as
  // trial, to identify each trial.
  useEffect(() => {
    if (from !== "behavior" || !behavior || (joinEpoch && joinBehavior)) return;
    const lower = (v: string) => v.toLowerCase();
    const shared = fields.filter((f) =>
      behavior.columns.some((c) => lower(c) === lower(f)),
    );
    const field = shared.find((f) => lower(f) === "trial") ?? shared[0];
    if (field) {
      setJoinEpoch(field);
      setJoinBehavior(
        behavior.columns.find((c) => lower(c) === lower(field)) ?? "",
      );
    }
  }, [from, behavior, fields]);
  useEffect(() => {
    if (!key || !grouped || needsJoin) {
      setSplit(null);
      return;
    }
    let alive = true;
    setLoading(true);
    window.eyeris
      .split({
        key,
        scope,
        stage,
        include,
        by: from ? { from, column } : null,
        join:
          from === "behavior"
            ? { epoch: joinEpoch, behavior: joinBehavior }
            : null,
      })
      .then((value) => {
        if (alive) {
          setSplit(value);
          setError("");
        }
      })
      .catch((e) => {
        if (alive) {
          setSplit(null);
          setError(e.message);
        }
      })
      .finally(() => {
        if (alive) setLoading(false);
      });
    return () => {
      alive = false;
    };
  }, [
    key,
    stage,
    include,
    scope,
    splitBy,
    joinEpoch,
    joinBehavior,
    project.counts.keep,
    project.counts.exclude,
  ]);
  // Long averages and links report how far they are.
  useEffect(() => {
    if (!loading && !linking) {
      setActivity({});
      return;
    }
    let alive = true;
    const poll = () =>
      window.eyeris
        .diagnosticsProgress()
        .then((p) => alive && setActivity(p))
        .catch(() => {});
    void poll();
    const timer = setInterval(poll, 300);
    return () => {
      alive = false;
      clearInterval(timer);
    };
  }, [loading, linking]);
  async function linkBehavior() {
    setLinking(true);
    try {
      const linked = await window.eyeris.linkBehavior();
      if (linked) {
        setBehavior(linked);
        setError("");
      }
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e));
    } finally {
      setLinking(false);
    }
  }
  useEffect(() => {
    if (!key || grouped) return;
    let alive = true;
    setLoading(true);
    window.eyeris
      .average({ key, stage, include })
      .then((value) => {
        if (alive) {
          setAverage(value);
          setError("");
        }
      })
      .catch((e) => {
        if (alive) setError(e.message);
      })
      .finally(() => {
        if (alive) setLoading(false);
      });
    return () => {
      alive = false;
    };
    // Decisions change which epochs are included.
  }, [
    key,
    stage,
    include,
    grouped,
    project.counts.keep,
    project.counts.exclude,
  ]);
  const index = groups.findIndex((g) => g.key === key);
  const group = groups[index];
  return (
    <div className="app-shell">
      <aside className="sidebar">
        <div className="brand">
          <img src="./sticker.png" alt="eyeris" />
          <strong>eyeris</strong>
        </div>
        <div className="sidebar-project">{project.name}</div>
        <button className="nav-item" onClick={onSubjects}>
          <Database size={17} /> Subjects & processing
        </button>
        <button className="nav-item" onClick={() => onReview()}>
          <Layers3 size={17} /> Epoch review{" "}
          <span className="nav-count">{project.counts.total}</span>
        </button>
        <div className="nav-item active">
          <LineChart size={17} /> Diagnostics
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
            Workspace <ChevronRight size={13} /> <span>Diagnostics</span>
          </div>
        </header>
        <div className="content diagnostics">
          <div className="page-heading">
            <div>
              <h1>Run diagnostics</h1>
              <p>
                A run's average with every epoch's trace behind it, or epochs
                split by an event field or trial-level behavior and pooled
                across runs and subjects.
              </p>
            </div>
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
          {!groups.length ? (
            <section className="welcome">
              <h2>No epochs in this project</h2>
              <p>
                Process recordings with epoching enabled, or import epoched
                eyeris objects, to see run averages.
              </p>
            </section>
          ) : (
            <>
              <div className="diagnostics-controls">
                <div className="run-picker">
                  <button
                    aria-label="Previous run"
                    disabled={index <= 0}
                    onClick={() => setKey(groups[index - 1].key)}
                  >
                    <ChevronLeft size={17} />
                  </button>
                  <select
                    aria-label="Run"
                    value={key}
                    onChange={(e) => setKey(e.target.value)}
                  >
                    {groups.map((g) => (
                      <option key={g.key} value={g.key}>
                        {groupName(g, groups)} ({number(g.epochs)})
                      </option>
                    ))}
                  </select>
                  <button
                    aria-label="Next run"
                    disabled={index < 0 || index >= groups.length - 1}
                    onClick={() => setKey(groups[index + 1].key)}
                  >
                    <ChevronRight size={17} />
                  </button>
                </div>
                <label>
                  Stage
                  <select
                    aria-label="Stage"
                    value={stage}
                    onChange={(e) => setStage(e.target.value)}
                  >
                    <option value="final">Final available stage</option>
                    {project.stages.map((s) => (
                      <option key={s} value={s}>
                        {stageName(s)} · {s.replace(/^pupil_/, "")}
                      </option>
                    ))}
                  </select>
                </label>
                <label>
                  Epochs
                  <select
                    aria-label="Epochs to average"
                    value={include}
                    onChange={(e) => setInclude(e.target.value as Include)}
                  >
                    <option value="included">Kept and unreviewed</option>
                    <option value="kept">Kept only</option>
                    <option value="all">All, including excluded</option>
                  </select>
                </label>
                <label className="check-label">
                  <input
                    type="checkbox"
                    checked={showTraces && !grouped}
                    disabled={grouped}
                    onChange={(e) => setShowTraces(e.target.checked)}
                  />{" "}
                  Show each epoch
                </label>
              </div>
              <div className="diagnostics-controls split-controls">
                <label>
                  Split by
                  <select
                    aria-label="Split by"
                    value={splitBy}
                    onChange={(e) => setSplitBy(e.target.value)}
                  >
                    <option value="">No split</option>
                    {!!fields.length && (
                      <optgroup label="Epoch fields">
                        {fields.map((f) => (
                          <option key={f} value={`epoch:${f}`}>
                            {f}
                          </option>
                        ))}
                      </optgroup>
                    )}
                    {!!behavior?.columns.length && (
                      <optgroup label="Behavioral columns">
                        {behavior.columns.map((c) => (
                          <option key={c} value={`behavior:${c}`}>
                            {c}
                          </option>
                        ))}
                      </optgroup>
                    )}
                  </select>
                </label>
                {from === "behavior" && (
                  <>
                    <label>
                      Match epoch field
                      <select
                        aria-label="Epoch field to match"
                        value={joinEpoch}
                        onChange={(e) => setJoinEpoch(e.target.value)}
                      >
                        <option value="">Choose…</option>
                        {fields.map((f) => (
                          <option key={f}>{f}</option>
                        ))}
                      </select>
                    </label>
                    <label>
                      to behavioral column
                      <select
                        aria-label="Behavioral column to match"
                        value={joinBehavior}
                        onChange={(e) => setJoinBehavior(e.target.value)}
                      >
                        <option value="">Choose…</option>
                        {behavior?.columns.map((c) => (
                          <option key={c}>{c}</option>
                        ))}
                      </select>
                    </label>
                  </>
                )}
                <label>
                  Average over
                  <select
                    aria-label="Average over"
                    value={scope}
                    onChange={(e) =>
                      setScope(e.target.value as SplitRequest["scope"])
                    }
                  >
                    <option value="run">This run</option>
                    <option value="subject">
                      {group ? `sub-${group.participant}, ` : ""}all runs
                    </option>
                    <option value="all">All subjects</option>
                  </select>
                </label>
                <div className="behavior-status">
                  {behavior?.rows
                    ? `Behavioral data: ${number(behavior.rows)} rows in ${number(behavior.files)} file${behavior.files === 1 ? "" : "s"}`
                    : "No behavioral data linked"}
                  <button
                    className="button"
                    disabled={linking}
                    onClick={() => void linkBehavior()}
                  >
                    {linking ? "Linking…" : "Link behavioral data…"}
                  </button>
                  {linking && activity.link && (
                    <ProgressLine progress={activity.link} />
                  )}
                </div>
              </div>
              <section className="signal-panel diagnostics-panel">
                <div className="plot-heading">
                  <span>
                    <i />
                    {grouped
                      ? `${scope === "all" ? "All subjects" : scope === "subject" ? `sub-${group?.participant}, all runs` : group ? groupName(group, groups) : "Run"} · ${group?.label.replace(/^epoch_/, "") ?? ""} epochs`
                      : group
                        ? groupName(group, groups)
                        : "Run"}{" "}
                    ·{" "}
                    {grouped
                      ? split
                        ? stageName(split.stage)
                        : "…"
                      : average
                        ? stageName(average.stage)
                        : "…"}
                  </span>
                  {loading && (
                    <span className="plot-loading">
                      <ProgressLine progress={progress} compact />
                      <LoaderCircle size={14} className="spin" />
                    </span>
                  )}
                </div>
                {grouped && split?.time && split.series.length ? (
                  <AveragePlot
                    time={split.time}
                    series={split.series.map((s, i) => ({
                      label: from ? `${column} = ${s.label}` : s.label,
                      color: palette[i],
                      mean: s.mean,
                      se: s.se,
                      n: s.n,
                    }))}
                    onset={!!split.onset}
                    showTraces={false}
                  />
                ) : !grouped && average?.time && average.mean ? (
                  <AveragePlot
                    time={average.time}
                    traces={average.traces}
                    series={[
                      {
                        label: "Mean",
                        color: "#820000",
                        mean: average.mean,
                        se: average.se,
                        n: average.epochs,
                      },
                    ]}
                    onset={!!average.onset}
                    showTraces={showTraces}
                  />
                ) : (
                  <div className="plot-placeholder">
                    {loading ? (
                      <>
                        <LoaderCircle className="spin" size={24} />
                        {progress ? (
                          <ProgressLine progress={progress} />
                        ) : (
                          <span>Averaging epochs…</span>
                        )}
                      </>
                    ) : (
                      <>
                        <Activity size={30} />
                        <span>
                          {needsJoin
                            ? "Choose the epoch field and behavioral column that identify each trial."
                            : "No epochs to average with this selection."}
                        </span>
                      </>
                    )}
                  </div>
                )}
                {grouped && split && (
                  <div className="signal-metadata">
                    <span>
                      <strong>{number(split.epochs)}</strong> of{" "}
                      {number(split.total)} epochs in {split.series.length}{" "}
                      group
                      {split.series.length === 1 ? "" : "s"}, pooled
                    </span>
                    {split.series.map((s, i) => (
                      <span key={s.label} className="split-legend">
                        <i style={{ background: palette[i] }} />
                        {from ? `${column} = ${s.label}` : s.label} (n ={" "}
                        {number(s.n)})
                      </span>
                    ))}
                    {[
                      [split.unmatched, "without a behavioral match"],
                      [split.ambiguous, "matching several behavioral rows"],
                      [split.missing, `without a value for ${column}`],
                    ]
                      .filter(([count]) => count)
                      .map(([count, text]) => (
                        <span key={text as string} className="split-dropped">
                          {number(count as number)} {text}
                        </span>
                      ))}
                  </div>
                )}
                {!grouped && group && (
                  <div className="signal-metadata">
                    <span>
                      <strong>{number(average?.epochs ?? 0)}</strong> of{" "}
                      {number(group.epochs)} epochs averaged
                    </span>
                    <span>
                      <strong>{number(group.keep)}</strong> kept ·{" "}
                      <strong>{number(group.exclude)}</strong> excluded ·{" "}
                      <strong>{number(group.unreviewed)}</strong> to review
                    </span>
                    <span>
                      <i className="legend-mean" /> Mean ± standard error
                      {showTraces && (
                        <>
                          {" "}
                          <i className="legend-trace" /> Each epoch
                        </>
                      )}
                    </span>
                    <button
                      className="link-button"
                      onClick={() => onReview(group.participant)}
                    >
                      Review sub-{group.participant}
                    </button>
                  </div>
                )}
              </section>
            </>
          )}
        </div>
      </main>
    </div>
  );
}
