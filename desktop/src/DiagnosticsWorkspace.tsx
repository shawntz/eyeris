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
  type DiagnosticGroup,
  type Include,
  type Summary,
} from "./types";

const number = (n: number) => n.toLocaleString();
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
  }, [project.sources.length, project.counts.total]);
  useEffect(() => {
    if (!key) return;
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
  }, [key, stage, include, project.counts.keep, project.counts.exclude]);
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
                The average of a run's epochs, with every epoch's trace behind
                it.
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
                    checked={showTraces}
                    onChange={(e) => setShowTraces(e.target.checked)}
                  />{" "}
                  Show each epoch
                </label>
              </div>
              <section className="signal-panel diagnostics-panel">
                <div className="plot-heading">
                  <span>
                    <i />
                    {group ? groupName(group, groups) : "Run"} ·{" "}
                    {average ? stageName(average.stage) : "…"}
                  </span>
                  {loading && <LoaderCircle size={14} className="spin" />}
                </div>
                {average?.time && average.mean ? (
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
                        <span>Averaging epochs…</span>
                      </>
                    ) : (
                      <>
                        <Activity size={30} />
                        <span>No epochs to average with this selection.</span>
                      </>
                    )}
                  </div>
                )}
                {group && (
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
