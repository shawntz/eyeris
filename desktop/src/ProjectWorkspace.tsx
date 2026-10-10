import { useState, useEffect } from "react";
import {
  Plus,
  FolderOpen,
  Layers3,
  Database,
  Play,
  Square,
  ChevronDown,
  FileText,
  Check,
  X,
  LoaderCircle,
} from "lucide-react";
import type {
  Summary,
  PipelineState,
  PipelineSettings,
  Recording,
} from "./types";
const steps = [
  {
    key: "resample",
    label: "Resample",
    description: "Regularize the sampling interval",
    params: {},
  },
  {
    key: "deblink",
    label: "Remove blinks",
    description: "Remove blinks and surrounding samples",
    params: { extend: 50 },
  },
  {
    key: "detransient",
    label: "Remove transients",
    description: "Detect abrupt changes in pupil size",
    params: { n: 16, mad_thresh: null },
  },
  {
    key: "interpolate",
    label: "Interpolate",
    description: "Fill short gaps in the signal",
    params: { max_gap_ms: 250 },
  },
  {
    key: "lpfilt",
    label: "Low-pass filter",
    description: "Attenuate high-frequency noise",
    params: { wp: 4, ws: 8, rp: 1, rs: 35, plot_freqz: false },
  },
  {
    key: "downsample",
    label: "Downsample",
    description: "Reduce the sampling frequency",
    params: { target_fs: 100 },
  },
  {
    key: "bin",
    label: "Bin samples",
    description: "Aggregate samples into time bins",
    params: { bins_per_second: 10, method: "mean" },
  },
  {
    key: "detrend",
    label: "Detrend",
    description: "Remove linear or spline trends",
    params: { method: "linear", spline_df: 5 },
  },
  {
    key: "zscore",
    label: "Standardize",
    description: "Convert to z-scores",
    params: {},
  },
];
const defaults: PipelineSettings = {
  glassbox: {
    load_asc: { block: "auto", binocular_mode: "average" },
    resample: true,
    deblink: { extend: 50 },
    detransient: { n: 16, mad_thresh: null },
    interpolate: { max_gap_ms: 250 },
    lpfilt: { wp: 4, ws: 8, rp: 1, rs: 35, plot_freqz: false },
    downsample: false,
    bin: false,
    detrend: false,
    zscore: true,
    seed: 123,
  },
  epoch: null,
  report: true,
  database: false,
};
const bidsName = (r: Recording) =>
  `sub-${r.subject}_ses-${r.session}_task-${r.task}${r.run ? `_run-${r.run}` : ""}`;
const names: Record<string, string> = {
  extend: "Blink padding (ms)",
  n: "Window samples",
  mad_thresh: "MAD threshold (auto if empty)",
  max_gap_ms: "Maximum gap (ms)",
  wp: "Passband (Hz)",
  ws: "Stopband (Hz)",
  rp: "Passband ripple (dB)",
  rs: "Stopband attenuation (dB)",
  target_fs: "Target frequency (Hz)",
  bins_per_second: "Bins per second",
  method: "Method",
  spline_df: "Spline degrees of freedom",
};
export function ProjectWorkspace({
  project,
  pipeline,
  onPipeline,
  onReview,
  onClose,
  onProject,
}: {
  project: Summary;
  pipeline: PipelineState;
  onPipeline: (p: PipelineState) => void;
  onReview: (participant?: string) => void;
  onClose: () => void;
  onProject: (p: Summary) => void;
}) {
  const [subject, setSubject] = useState("");
  const [newSubject, setNewSubject] = useState("");
  const [session, setSession] = useState("01");
  const [task, setTask] = useState("");
  const [run, setRun] = useState("");
  const [recording, setRecording] = useState("");
  const [checked, setChecked] = useState<string[]>([]);
  const [settings, setSettings] = useState<PipelineSettings>(
    structuredClone(defaults),
  );
  const [openStep, setOpenStep] = useState("");
  const [log, setLog] = useState("");
  const [error, setError] = useState("");
  const [busy, setBusy] = useState(false);
  const [showLog, setShowLog] = useState(false);
  const records = pipeline.recordings.filter((r) => r.subject === subject);
  const latestJob = (id: string) =>
    pipeline.jobs.find((j) => j.recordings.includes(id));
  const jobs = pipeline.jobs.filter((j) => j.recordings.includes(recording));
  const latest = jobs[0];
  const active = pipeline.active;
  const running = !!active;
  const selected = records.filter((r) => checked.includes(r.id));
  const named = (id: string) =>
    pipeline.recordings.find((r) => r.id === id) as Recording | undefined;
  // Runs left out of a session's job are missing from its report and database.
  const partial = [...new Set(selected.map((r) => `${r.session}/${r.task}`))]
    .map((key) => {
      const all = records.filter((r) => `${r.session}/${r.task}` === key);
      const [ses, tsk] = key.split("/");
      const count = selected.filter((r) => all.includes(r)).length;
      return count < all.length
        ? `ses-${ses} task-${tsk}: ${count} of ${all.length} runs selected.`
        : "";
    })
    .filter(Boolean);
  const nextRun = String(
    Math.max(
      0,
      ...records
        .filter((r) => r.session === session && r.task === task)
        .map((r) => Number(r.run) || 1),
    ) + 1,
  ).padStart(2, "0");
  useEffect(() => {
    if (!subject && pipeline.subjects[0]) setSubject(pipeline.subjects[0].id);
  }, [pipeline.subjects.length]);
  useEffect(() => {
    // Start with this subject's recordings that have not been processed.
    setChecked(
      records
        .filter((r) => latestJob(r.id)?.status !== "completed")
        .map((r) => r.id),
    );
    setRecording(records[0]?.id || "");
  }, [subject]);
  useEffect(() => {
    if (!records.some((r) => r.id === recording))
      setRecording(records[0]?.id || "");
  }, [records.length]);
  useEffect(() => {
    if (pipeline.active) setLog(pipeline.active.log);
  }, [pipeline.active?.log]);
  useEffect(() => {
    if (latest && !running)
      window.eyeris
        .pipelineLog(latest.id)
        .then(setLog)
        .catch(() => {});
  }, [latest?.id, latest?.status, running]);
  async function act(fn: () => Promise<void>) {
    if (busy) return;
    setBusy(true);
    setError("");
    try {
      await fn();
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e));
    } finally {
      setBusy(false);
    }
  }
  function setStep(key: string, value: PipelineSettings["glassbox"][string]) {
    setSettings((s) => ({ ...s, glassbox: { ...s.glassbox, [key]: value } }));
  }
  function epoch(change: Partial<NonNullable<PipelineSettings["epoch"]>>) {
    setSettings((s) => ({ ...s, epoch: { ...s.epoch!, ...change } }));
  }
  async function importProcessed() {
    await act(async () => {
      const r = await window.eyeris.importFiles();
      if (r) {
        onProject(r.project);
        const failures = r.results.filter((x) => x.error);
        if (failures.length) setError(failures.map((x) => x.error).join("\n"));
        else onReview();
      }
    });
  }
  return (
    <div className="app-shell">
      <aside className="sidebar">
        <div className="brand">
          <img src="./sticker.png" alt="eyeris" />
          <strong>eyeris</strong>
        </div>
        <div className="sidebar-project">{project.name}</div>
        <div className="nav-item active">
          <Database size={17} /> Subjects & processing
        </div>
        <button className="nav-item" onClick={() => onReview()}>
          <Layers3 size={17} /> Epoch review{" "}
          <span className="nav-count">{project.counts.total}</span>
        </button>
        <div className="subject-list">
          <h2>Subjects</h2>
          {pipeline.subjects.map((s) => (
            <button
              className={s.id === subject ? "selected" : ""}
              key={s.id}
              onClick={() => setSubject(s.id)}
            >
              sub-{s.id}
              <span>
                {pipeline.recordings.filter((r) => r.subject === s.id).length}
              </span>
            </button>
          ))}
          <form
            onSubmit={(e) => {
              e.preventDefault();
              void act(async () => {
                onPipeline(await window.eyeris.addSubject(newSubject));
                setSubject(newSubject);
                setNewSubject("");
              });
            }}
          >
            <label htmlFor="new-subject">New subject ID</label>
            <div>
              <input
                id="new-subject"
                placeholder="001"
                value={newSubject}
                onChange={(e) => setNewSubject(e.target.value)}
                pattern="[a-zA-Z0-9]+"
                required
              />
              <button
                className="button"
                aria-label="Create subject"
                disabled={busy}
              >
                <Plus size={16} />
              </button>
            </div>
          </form>
        </div>
        <div className="sidebar-bottom">
          <button onClick={onClose}>
            <FolderOpen size={15} /> Projects
          </button>
        </div>
      </aside>
      <main>
        <header className="topbar">
          <div>
            {project.name}
            <span className="path-label">{project.directory}</span>
          </div>
          <button
            onClick={() =>
              void act(() => window.eyeris.showProjectFiles("project"))
            }
          >
            <FolderOpen size={15} /> Show files
          </button>
        </header>
        <div className="processing-content">
          <div className="page-heading">
            <div>
              <h1>{subject ? `sub-${subject}` : "Subjects & processing"}</h1>
              <p>
                {subject
                  ? "Recordings and preprocessing"
                  : "Create a subject to add an EyeLink recording."}
              </p>
            </div>
            <button
              className="button"
              disabled={busy}
              onClick={() => void importProcessed()}
            >
              Import processed RDS
            </button>
          </div>
          {error && (
            <div className="message error" role="alert">
              {error}
              <button onClick={() => setError("")} aria-label="Dismiss error">
                <X size={15} />
              </button>
            </div>
          )}
          {!subject ? (
            <div className="project-empty">
              <h2>Start with a subject</h2>
              <p>
                Enter a subject ID in the sidebar, then add its .asc recording.
              </p>
              <p>
                You can also import an already processed eyeris object for epoch
                review.
              </p>
            </div>
          ) : (
            <>
              <section className="recordings-section">
                <div className="section-heading">
                  <h2>Recordings</h2>
                  <small>
                    Add one ASC per run. Select several files to add consecutive
                    runs.
                  </small>
                </div>
                {!!records.length && (
                  <table className="recordings-table">
                    <thead>
                      <tr>
                        <th className="select-column">
                          <input
                            type="checkbox"
                            aria-label="Select all recordings"
                            disabled={running}
                            checked={selected.length === records.length}
                            ref={(el) => {
                              if (el)
                                el.indeterminate =
                                  !!selected.length &&
                                  selected.length < records.length;
                            }}
                            onChange={(e) =>
                              setChecked(
                                e.target.checked
                                  ? records.map((r) => r.id)
                                  : [],
                              )
                            }
                          />
                        </th>
                        <th>Recording</th>
                        <th>Session</th>
                        <th>Task</th>
                        <th>Run</th>
                        <th>Status</th>
                      </tr>
                    </thead>
                    <tbody>
                      {records.map((r) => {
                        const position = active
                          ? active.recordings.indexOf(r.id)
                          : -1;
                        const status =
                          !active || position < 0
                            ? latestJob(r.id)?.status || "ready"
                            : r.id === active.current
                              ? "running"
                              : position <
                                  active.recordings.indexOf(active.current)
                                ? "processed"
                                : "queued";
                        return (
                          <tr
                            key={r.id}
                            className={recording === r.id ? "selected" : ""}
                          >
                            <td className="select-column">
                              <input
                                type="checkbox"
                                aria-label={`Process ${bidsName(r)}`}
                                disabled={running}
                                checked={checked.includes(r.id)}
                                onChange={(e) =>
                                  setChecked((c) =>
                                    e.target.checked
                                      ? [...c, r.id]
                                      : c.filter((id) => id !== r.id),
                                  )
                                }
                              />
                            </td>
                            <td>
                              <button onClick={() => setRecording(r.id)}>
                                <FileText size={15} />
                                {r.name}
                              </button>
                            </td>
                            <td>{r.session}</td>
                            <td>{r.task}</td>
                            <td>
                              {r.run || (
                                <span title="eyeris numbers the blocks in this ASC as runs">
                                  Blocks
                                </span>
                              )}
                            </td>
                            <td>
                              <span className={`job-status ${status}`}>
                                {status}
                              </span>
                            </td>
                          </tr>
                        );
                      })}
                    </tbody>
                  </table>
                )}
                <form
                  className="add-recording"
                  onSubmit={(e) => {
                    e.preventDefault();
                    void act(async () => {
                      const before = new Set(
                        pipeline.recordings.map((r) => r.id),
                      );
                      const r = await window.eyeris.addRecording({
                        subject,
                        session,
                        task,
                        run,
                      });
                      if (r) {
                        onPipeline(r);
                        const added = r.recordings
                          .filter((x) => !before.has(x.id))
                          .map((x) => x.id);
                        setChecked((c) => [...c, ...added]);
                        setRecording(added[0]);
                        setRun("");
                      }
                    });
                  }}
                >
                  <label>
                    Session
                    <input
                      aria-label="Session"
                      value={session}
                      onChange={(e) => setSession(e.target.value)}
                      pattern="[a-zA-Z0-9]+"
                      required
                    />
                  </label>
                  <label>
                    Task
                    <input
                      aria-label="Task"
                      placeholder="memory"
                      value={task}
                      onChange={(e) => setTask(e.target.value)}
                      pattern="[a-zA-Z0-9]+"
                      required
                    />
                  </label>
                  <label className="run-field">
                    First run
                    <input
                      aria-label="First run"
                      placeholder={nextRun}
                      value={run}
                      onChange={(e) => setRun(e.target.value)}
                      pattern="[0-9]{1,3}"
                      inputMode="numeric"
                    />
                  </label>
                  <button className="button" disabled={busy || running}>
                    <Plus size={16} /> Add ASC files
                  </button>
                </form>
              </section>
              {!!records.length && (
                <div className="pipeline-layout">
                  <section className="pipeline-config">
                    <div className="section-heading">
                      <h2>Glassbox pipeline</h2>
                      <button
                        onClick={() => setSettings(structuredClone(defaults))}
                        disabled={running}
                      >
                        Reset defaults
                      </button>
                    </div>
                    <fieldset disabled={running || busy}>
                      <div className="load-options">
                        <label>
                          Eye
                          <select
                            aria-label="Eye"
                            value={String(
                              (
                                settings.glassbox.load_asc as Record<
                                  string,
                                  unknown
                                >
                              ).binocular_mode,
                            )}
                            onChange={(e) =>
                              setStep("load_asc", {
                                ...(settings.glassbox.load_asc as object),
                                binocular_mode: e.target.value,
                              })
                            }
                          >
                            <option value="average">Average</option>
                            <option value="left">Left</option>
                            <option value="right">Right</option>
                            <option value="both">Both, separately</option>
                          </select>
                        </label>
                        <label>
                          Random seed
                          <input
                            type="number"
                            aria-label="Random seed"
                            value={Number(settings.glassbox.seed)}
                            onChange={(e) =>
                              setStep("seed", Number(e.target.value))
                            }
                          />
                        </label>
                      </div>
                      <div className="pipeline-steps">
                        {steps.map((s, i) => {
                          const enabled = !!settings.glassbox[s.key];
                          const params: Record<string, unknown> =
                            typeof settings.glassbox[s.key] === "object"
                              ? (settings.glassbox[s.key] as Record<
                                  string,
                                  unknown
                                >)
                              : s.params;
                          return (
                            <div className="pipeline-step" key={s.key}>
                              <div className="step-line">
                                <span className="step-number">
                                  {String(i + 1).padStart(2, "0")}
                                </span>
                                <label>
                                  <input
                                    type="checkbox"
                                    checked={enabled}
                                    onChange={(e) =>
                                      setStep(
                                        s.key,
                                        e.target.checked
                                          ? Object.keys(s.params).length
                                            ? { ...s.params }
                                            : true
                                          : false,
                                      )
                                    }
                                  />
                                  <span>
                                    {s.label}
                                    <small>{s.description}</small>
                                  </span>
                                </label>
                                {!!Object.keys(s.params).length && (
                                  <button
                                    aria-label={`${s.label} parameters`}
                                    type="button"
                                    onClick={() =>
                                      setOpenStep(
                                        openStep === s.key ? "" : s.key,
                                      )
                                    }
                                  >
                                    <ChevronDown size={15} />
                                  </button>
                                )}
                              </div>
                              {openStep === s.key && enabled && (
                                <div className="step-parameters">
                                  {Object.entries(params)
                                    .filter(([key]) => key !== "plot_freqz")
                                    .map(([key, value]) => (
                                      <label key={key}>
                                        {names[key] || key}
                                        {typeof value === "string" ? (
                                          <select
                                            value={value}
                                            onChange={(e) =>
                                              setStep(s.key, {
                                                ...params,
                                                [key]: e.target.value,
                                              })
                                            }
                                          >
                                            {(s.key === "bin"
                                              ? ["mean", "median"]
                                              : ["linear", "spline"]
                                            ).map((v) => (
                                              <option key={v}>{v}</option>
                                            ))}
                                          </select>
                                        ) : (
                                          <input
                                            type="number"
                                            step="any"
                                            aria-label={names[key] || key}
                                            value={
                                              value === null
                                                ? ""
                                                : Number(value)
                                            }
                                            onChange={(e) =>
                                              setStep(s.key, {
                                                ...params,
                                                [key]:
                                                  e.target.value === ""
                                                    ? null
                                                    : Number(e.target.value),
                                              })
                                            }
                                          />
                                        )}
                                      </label>
                                    ))}
                                </div>
                              )}
                            </div>
                          );
                        })}
                      </div>
                      <details className="advanced-config">
                        <summary>Advanced glassbox options</summary>
                        <p>
                          Override supported step parameters using the package’s
                          argument names.
                        </p>
                        <textarea
                          aria-label="Advanced glassbox options"
                          key={JSON.stringify(settings.glassbox)}
                          defaultValue={JSON.stringify(
                            settings.glassbox,
                            null,
                            2,
                          )}
                          onBlur={(e) => {
                            try {
                              const parsed = JSON.parse(e.target.value);
                              if (
                                !parsed.load_asc ||
                                typeof parsed.load_asc !== "object"
                              )
                                throw new Error();
                              setSettings((s) => ({ ...s, glassbox: parsed }));
                              setError("");
                            } catch {
                              setError(
                                "Enter a valid JSON settings object including load_asc.",
                              );
                            }
                          }}
                        />
                      </details>
                    </fieldset>
                  </section>
                  <section className="pipeline-output">
                    <h2>Epochs & output</h2>
                    <fieldset disabled={running || busy}>
                      <label className="check-label">
                        <input
                          type="checkbox"
                          aria-label="Extract epochs"
                          checked={!!settings.epoch}
                          onChange={(e) =>
                            setSettings((s) => ({
                              ...s,
                              epoch: e.target.checked
                                ? {
                                    events: "",
                                    limits: [-1, 2],
                                    label: "trial",
                                    baseline: false,
                                  }
                                : null,
                            }))
                          }
                        />{" "}
                        Extract epochs for review
                      </label>
                      {settings.epoch && (
                        <div className="epoch-options">
                          <label>
                            Event pattern
                            <input
                              aria-label="Event pattern"
                              placeholder="PROBE_START_{trial}"
                              value={settings.epoch.events}
                              onChange={(e) =>
                                epoch({ events: e.target.value })
                              }
                            />
                          </label>
                          <label>
                            Epoch label
                            <input
                              aria-label="Epoch label"
                              value={settings.epoch.label}
                              onChange={(e) => epoch({ label: e.target.value })}
                            />
                          </label>
                          <div className="paired-fields">
                            <label>
                              Start (s)
                              <input
                                type="number"
                                step="any"
                                aria-label="Epoch start"
                                value={settings.epoch.limits?.[0] ?? -1}
                                onChange={(e) =>
                                  epoch({
                                    limits: [
                                      Number(e.target.value),
                                      settings.epoch!.limits?.[1] ?? 2,
                                    ],
                                  })
                                }
                              />
                            </label>
                            <label>
                              End (s)
                              <input
                                type="number"
                                step="any"
                                aria-label="Epoch end"
                                value={settings.epoch.limits?.[1] ?? 2}
                                onChange={(e) =>
                                  epoch({
                                    limits: [
                                      settings.epoch!.limits?.[0] ?? -1,
                                      Number(e.target.value),
                                    ],
                                  })
                                }
                              />
                            </label>
                          </div>
                          <label className="check-label">
                            <input
                              type="checkbox"
                              checked={settings.epoch.baseline}
                              onChange={(e) =>
                                epoch({
                                  baseline: e.target.checked,
                                  baseline_type: "sub",
                                  baseline_period: [-1, 0],
                                })
                              }
                            />{" "}
                            Baseline correction
                          </label>
                          {settings.epoch.baseline && (
                            <>
                              <label>
                                Method
                                <select
                                  value={settings.epoch.baseline_type}
                                  onChange={(e) =>
                                    epoch({ baseline_type: e.target.value })
                                  }
                                >
                                  <option value="sub">Subtract baseline</option>
                                  <option value="div">
                                    Divide by baseline
                                  </option>
                                </select>
                              </label>
                              <div className="paired-fields">
                                <label>
                                  Baseline start (s)
                                  <input
                                    type="number"
                                    step="any"
                                    value={
                                      settings.epoch.baseline_period?.[0] ?? -1
                                    }
                                    onChange={(e) =>
                                      epoch({
                                        baseline_period: [
                                          Number(e.target.value),
                                          settings.epoch!
                                            .baseline_period?.[1] ?? 0,
                                        ],
                                      })
                                    }
                                  />
                                </label>
                                <label>
                                  Baseline end (s)
                                  <input
                                    type="number"
                                    step="any"
                                    value={
                                      settings.epoch.baseline_period?.[1] ?? 0
                                    }
                                    onChange={(e) =>
                                      epoch({
                                        baseline_period: [
                                          settings.epoch!
                                            .baseline_period?.[0] ?? -1,
                                          Number(e.target.value),
                                        ],
                                      })
                                    }
                                  />
                                </label>
                              </div>
                            </>
                          )}
                        </div>
                      )}
                      <div className="output-options">
                        <span>Save to this project</span>
                        <div>
                          <Check size={14} /> BIDS CSV files and RDS result
                        </div>
                        <label className="check-label">
                          <input
                            type="checkbox"
                            checked={settings.report}
                            onChange={(e) =>
                              setSettings((s) => ({
                                ...s,
                                report: e.target.checked,
                              }))
                            }
                          />{" "}
                          HTML diagnostic report
                        </label>
                        <label className="check-label">
                          <input
                            type="checkbox"
                            checked={settings.database}
                            onChange={(e) =>
                              setSettings((s) => ({
                                ...s,
                                database: e.target.checked,
                              }))
                            }
                          />{" "}
                          DuckDB database
                        </label>
                      </div>
                    </fieldset>
                    <div className="run-actions">
                      {running ? (
                        <button
                          className="button"
                          disabled={pipeline.active?.phase === "publishing"}
                          onClick={() =>
                            void act(async () =>
                              onPipeline(await window.eyeris.cancelPipeline()),
                            )
                          }
                        >
                          <Square size={14} /> Cancel processing
                        </button>
                      ) : (
                        <button
                          className="button primary"
                          disabled={
                            busy ||
                            !selected.length ||
                            (!!settings.epoch && !settings.epoch.events.trim())
                          }
                          onClick={() =>
                            void act(async () => {
                              onPipeline(
                                await window.eyeris.startPipeline(
                                  selected.map((r) => r.id),
                                  settings,
                                ),
                              );
                              if (!checked.includes(recording))
                                setRecording(selected[0].id);
                              setShowLog(true);
                            })
                          }
                        >
                          <Play size={15} />{" "}
                          {selected.length > 1
                            ? `Run pipeline on ${selected.length} recordings`
                            : "Run pipeline"}
                        </button>
                      )}
                    </div>
                    {!running && (
                      <p className="run-note">
                        {!selected.length
                          ? "Select the recordings to process."
                          : partial.length
                            ? `${partial.join(" ")} Its report and database will include only the selected runs.`
                            : selected.length > 1
                              ? "Selected recordings are processed in one job. Each session report and database covers all of its runs."
                              : ""}
                      </p>
                    )}
                    {running && (
                      <div className="run-progress">
                        <LoaderCircle size={15} className="spin" />{" "}
                        {active.recordings.length > 1 &&
                          `${active.recordings.indexOf(active.current) + 1} of ${active.recordings.length} · ${bidsName(named(active.current)!)} · `}
                        {active.phase === "starting"
                          ? "Starting R…"
                          : `Running ${active.phase}…`}
                      </div>
                    )}
                    {latest && !running && (
                      <div className="run-result">
                        <strong className={`job-status ${latest.status}`}>
                          {latest.status === "completed"
                            ? "Processing complete"
                            : latest.status}
                        </strong>
                        {latest.recordings.length > 1 && (
                          <p>
                            {latest.recordings.length} recordings in this job:{" "}
                            {latest.recordings
                              .map((id) => named(id))
                              .filter(Boolean)
                              .map((r) => bidsName(r!))
                              .join(", ")}
                          </p>
                        )}
                        {latest.error && (
                          <p role="alert">{latest.error.slice(-1500)}</p>
                        )}
                        {latest.status === "completed" && (
                          <>
                            <p>
                              {JSON.parse(latest.outputs).length
                                ? "BIDS output is available in the project’s bids folder."
                                : "This run’s BIDS output is saved separately to preserve earlier results."}
                            </p>
                            <button
                              className="button"
                              onClick={() =>
                                void act(() =>
                                  window.eyeris.showProjectFiles(
                                    "run",
                                    latest.id,
                                  ),
                                )
                              }
                            >
                              Show run output
                            </button>
                            {latest.config.epoch && (
                              <button
                                className="button primary"
                                onClick={() => onReview(subject)}
                              >
                                Review epochs
                              </button>
                            )}
                          </>
                        )}
                      </div>
                    )}
                    {!!jobs.length && (
                      <details className="run-history">
                        <summary>Processing history ({jobs.length})</summary>
                        {jobs.map((j) => (
                          <div key={j.id}>
                            <span>
                              {new Date(j.started_at).toLocaleString()} ·{" "}
                              {j.status}
                              {j.recordings.length > 1 &&
                                ` · ${j.recordings.length} recordings`}
                            </span>
                            <button
                              onClick={() =>
                                void act(() =>
                                  window.eyeris.showProjectFiles("run", j.id),
                                )
                              }
                            >
                              Files
                            </button>
                            <button
                              disabled={running}
                              onClick={() =>
                                setSettings(structuredClone(j.config))
                              }
                            >
                              Reuse settings
                            </button>
                          </div>
                        ))}
                      </details>
                    )}
                  </section>
                </div>
              )}
              {(latest || running) && (
                <section className="processing-log">
                  <button onClick={() => setShowLog(!showLog)}>
                    <ChevronDown size={15} /> Processing log
                  </button>
                  {showLog && (
                    <pre aria-label="Processing log">
                      {log || "Waiting for R…"}
                    </pre>
                  )}
                </section>
              )}
            </>
          )}
        </div>
      </main>
    </div>
  );
}
