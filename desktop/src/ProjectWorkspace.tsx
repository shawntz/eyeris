import {
  useState,
  useEffect,
  useRef,
  type Dispatch,
  type SetStateAction,
} from "react";
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
  FolderInput,
  Users,
} from "lucide-react";
import type {
  Summary,
  PipelineState,
  PipelineSettings,
  Recording,
} from "./types";
import { PipelineSettingsPanel } from "./PipelineSettingsPanel";
import { BatchProcessing } from "./BatchProcessing";
import { AutoExcludeControl } from "./AutoExcludeControl";
// The subject list entry that opens batch processing for every subject.
const ALL = "*";
const megabytes = (n: number) => `${(n / 1e6).toFixed(n < 1e7 ? 1 : 0)} MB`;
// Kept outside the component so a dismissed summary stays dismissed when the
// workspace is reopened.
let dismissedImport = "";
const bidsName = (r: Recording) =>
  `sub-${r.subject}_ses-${r.session}_task-${r.task}${r.run ? `_run-${r.run}` : ""}`;
export function ProjectWorkspace({
  project,
  pipeline,
  settings,
  setSettings,
  onPipeline,
  onReview,
  onClose,
  onProject,
}: {
  project: Summary;
  pipeline: PipelineState;
  settings: PipelineSettings;
  setSettings: Dispatch<SetStateAction<PipelineSettings>>;
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
  const [log, setLog] = useState("");
  const [error, setError] = useState("");
  const [busy, setBusy] = useState(false);
  const [showLog, setShowLog] = useState(false);
  const [, setDismissed] = useState(dismissedImport);
  const records = pipeline.recordings.filter((r) => r.subject === subject);
  const latestJob = (id: string) =>
    pipeline.jobs.find((j) => j.recordings.includes(id));
  const jobs = pipeline.jobs.filter((j) => j.recordings.includes(recording));
  const latest = jobs[0];
  const mine = (j: { recordings: string[] }) =>
    records.some((r) => j.recordings.includes(r.id));
  // This subject's running job, and any of its jobs waiting in the queue.
  const active = pipeline.running.find(mine);
  const waiting = pipeline.queued.filter(mine);
  const running = !!active || !!waiting.length;
  const importing = pipeline.importing;
  const imported =
    pipeline.lastImport?.id !== dismissedImport ? pipeline.lastImport : null;
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
  // Recordings added since the subject was opened, by hand or by a BIDS import
  // still in progress, are selected for processing.
  const seen = useRef(new Set<string>());
  useEffect(() => {
    const fresh = records.filter((r) => !seen.current.has(r.id));
    for (const r of fresh) seen.current.add(r.id);
    if (fresh.length)
      setChecked((c) => [
        ...new Set([
          ...c,
          ...fresh
            .filter((r) => latestJob(r.id)?.status !== "completed")
            .map((r) => r.id),
        ]),
      ]);
  }, [records.map((r) => r.id).join()]);
  useEffect(() => {
    if (active) setLog(active.log);
  }, [active?.log]);
  useEffect(() => {
    if (latest && !running)
      window.eyeris
        .pipelineLog(latest.id)
        .then(setLog)
        .catch(() => {});
  }, [latest?.id, latest?.status, running]);
  const exclusion = (
    <AutoExcludeControl project={project} onProject={onProject} />
  );
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
  function importBids() {
    void act(async () => {
      const r = await window.eyeris.importBids();
      if (r) onPipeline(r);
    });
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
          {!!pipeline.subjects.length && (
            <button
              className={`all-subjects ${subject === ALL ? "selected" : ""}`}
              onClick={() => setSubject(ALL)}
            >
              <Users size={14} /> All subjects
              <span>{pipeline.subjects.length}</span>
            </button>
          )}
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
              <h1>
                {subject === ALL
                  ? "All subjects"
                  : subject
                    ? `sub-${subject}`
                    : "Subjects & processing"}
              </h1>
              <p>
                {subject === ALL
                  ? "Set the pipeline once, then process every subject."
                  : subject
                    ? "Recordings and preprocessing"
                    : "Create a subject to add an EyeLink recording."}
              </p>
            </div>
            <div className="heading-actions">
              <button
                className="button"
                disabled={busy || running || !!importing}
                onClick={importBids}
              >
                <FolderInput size={16} /> Import BIDS folder
              </button>
              <button
                className="button"
                disabled={busy}
                onClick={() => void importProcessed()}
              >
                Import processed RDS
              </button>
            </div>
          </div>
          {importing && (
            <section className="import-progress" role="status">
              <div>
                <strong>
                  <LoaderCircle size={15} className="spin" /> Importing BIDS
                  recordings
                </strong>
                <span>
                  {importing.completed} of {importing.total} files ·{" "}
                  {megabytes(importing.bytes)} of{" "}
                  {megabytes(importing.totalBytes)}
                </span>
                <button
                  onClick={() =>
                    void act(async () =>
                      onPipeline(await window.eyeris.cancelImport()),
                    )
                  }
                >
                  Cancel
                </button>
              </div>
              <div
                className="progress-track"
                role="progressbar"
                aria-label="Import progress"
                aria-valuemin={0}
                aria-valuemax={100}
                aria-valuenow={Math.round(
                  (100 * importing.bytes) / (importing.totalBytes || 1),
                )}
              >
                <span
                  style={{
                    width: `${(100 * importing.bytes) / (importing.totalBytes || 1)}%`,
                  }}
                />
              </div>
              <small>{importing.current}</small>
            </section>
          )}
          {imported && !importing && (
            <div
              className={`message ${imported.error ? "error" : "notice"}`}
              role="status"
            >
              <div>
                <strong>
                  {imported.cancelled
                    ? "Import cancelled. "
                    : imported.error
                      ? "Import stopped. "
                      : ""}
                  Added {imported.added} recording
                  {imported.added === 1 ? "" : "s"}
                  {imported.added
                    ? ` for ${imported.subjects} subject${imported.subjects === 1 ? "" : "s"}`
                    : ""}
                  .
                </strong>
                {imported.error && <p>{imported.error}</p>}
                {imported.subjects > 1 && subject !== ALL && (
                  <button
                    className="link-button"
                    onClick={() => setSubject(ALL)}
                  >
                    Process all subjects
                  </button>
                )}
                {!!imported.skipped.length && (
                  <details>
                    <summary>
                      {imported.skipped.length} file
                      {imported.skipped.length === 1 ? "" : "s"} skipped
                    </summary>
                    <ul>
                      {imported.skipped.map((f) => (
                        <li key={f.file}>
                          {f.file}: {f.reason}
                        </li>
                      ))}
                    </ul>
                  </details>
                )}
              </div>
              <button
                aria-label="Dismiss import summary"
                onClick={() => {
                  dismissedImport = imported.id;
                  setDismissed(imported.id);
                }}
              >
                <X size={15} />
              </button>
            </div>
          )}
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
                Enter a subject ID in the sidebar, then add its .asc recordings.
              </p>
              <p>
                To add a whole study at once, import a BIDS folder. Every
                sub-*/[ses-*/]eye/*.asc file is added with its subject, session,
                task and run.
              </p>
              <p>
                You can also import an already processed eyeris object for epoch
                review.
              </p>
            </div>
          ) : subject === ALL ? (
            <BatchProcessing
              pipeline={pipeline}
              settings={settings}
              setSettings={setSettings}
              busy={busy}
              act={act}
              onPipeline={onPipeline}
              onReview={() => onReview()}
              onSubject={setSubject}
              onError={setError}
              epochExtras={exclusion}
            />
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
                            ? waiting.some((j) => j.recordings.includes(r.id))
                              ? "queued"
                              : latestJob(r.id)?.status || "ready"
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
                  <button
                    className="button"
                    disabled={busy || running || !!importing}
                  >
                    <Plus size={16} /> Add ASC files
                  </button>
                </form>
              </section>
              {!!records.length && (
                <PipelineSettingsPanel
                  settings={settings}
                  setSettings={setSettings}
                  disabled={busy}
                  onError={setError}
                  epochExtras={exclusion}
                >
                  <div className="run-actions">
                    {running ? (
                      <button
                        className="button"
                        disabled={
                          busy ||
                          (!waiting.length && active?.phase === "publishing")
                        }
                        onClick={() =>
                          void act(async () => {
                            for (const job of [
                              ...waiting,
                              ...(active ? [active] : []),
                            ])
                              onPipeline(
                                await window.eyeris.cancelPipeline(job.id),
                              );
                          })
                        }
                      >
                        <Square size={14} /> Cancel processing
                      </button>
                    ) : (
                      <button
                        className="button primary"
                        disabled={
                          busy ||
                          !!importing ||
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
                      {pipeline.running.length >= pipeline.parallel.jobs &&
                        " Other subjects are processing; this job will wait its turn."}
                    </p>
                  )}
                  {active && (
                    <div className="run-progress">
                      <LoaderCircle size={15} className="spin" />{" "}
                      {active.recordings.length > 1 &&
                        `${active.recordings.indexOf(active.current) + 1} of ${active.recordings.length} · ${bidsName(named(active.current)!)} · `}
                      {active.phase === "starting"
                        ? "Starting R…"
                        : `Running ${active.phase}…`}
                    </div>
                  )}
                  {!active && !!waiting.length && (
                    <div className="run-progress">
                      <LoaderCircle size={15} className="spin" /> Queued behind
                      other subjects…
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
                </PipelineSettingsPanel>
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
