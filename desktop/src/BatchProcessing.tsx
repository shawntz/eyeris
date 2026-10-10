import {
  useEffect,
  useRef,
  useState,
  type Dispatch,
  type SetStateAction,
  type ReactNode,
} from "react";
import { LoaderCircle, Play, Square } from "lucide-react";
import type { PipelineSettings, PipelineState, Recording } from "./types";
import { PipelineSettingsPanel } from "./PipelineSettingsPanel";

const plural = (n: number, word: string) => `${n} ${word}${n === 1 ? "" : "s"}`;
const bidsName = (r: Recording) =>
  `ses-${r.session}_task-${r.task}${r.run ? `_run-${r.run}` : ""}`;

// Process every subject with one set of settings. Each subject is its own job,
// with its runs processed in order in one R session.
export function BatchProcessing({
  pipeline,
  settings,
  setSettings,
  busy,
  act,
  onPipeline,
  onReview,
  onSubject,
  onError,
  epochExtras,
}: {
  pipeline: PipelineState;
  settings: PipelineSettings;
  setSettings: Dispatch<SetStateAction<PipelineSettings>>;
  busy: boolean;
  act: (fn: () => Promise<void>) => Promise<void>;
  onPipeline: (p: PipelineState) => void;
  onReview: () => void;
  onSubject: (id: string) => void;
  onError: (message: string) => void;
  epochExtras?: ReactNode;
}) {
  const latestJob = (id: string) =>
    pipeline.jobs.find((j) => j.recordings.includes(id));
  const pending = [...pipeline.running, ...pipeline.queued];
  const inFlight = new Set(pending.flatMap((j) => j.recordings));
  const subjects = pipeline.subjects.map((s) => {
    const records = pipeline.recordings.filter((r) => r.subject === s.id);
    const statuses = records.map((r) => latestJob(r.id)?.status ?? "ready");
    const done = statuses.filter((x) => x === "completed").length;
    const state = !records.length
      ? { kind: "empty", label: "No recordings" }
      : pipeline.running.some((j) =>
            records.some((r) => j.recordings.includes(r.id)),
          )
        ? { kind: "running", label: "Running" }
        : records.some((r) => inFlight.has(r.id))
          ? { kind: "queued", label: "Queued" }
          : done === records.length
            ? { kind: "completed", label: "Completed" }
            : (["failed", "interrupted", "cancelled"]
                .filter((x) => statuses.includes(x))
                .map((x) => ({ kind: x, label: x }))[0] ??
              (done
                ? {
                    kind: "partial",
                    label: `${done} of ${records.length} processed`,
                  }
                : { kind: "ready", label: "Ready" }));
    return { id: s.id, records, state };
  });
  const available = (s: (typeof subjects)[number]) =>
    !!s.records.length && !s.records.some((r) => inFlight.has(r.id));
  // Subjects with unprocessed recordings start selected, including subjects
  // that appear while a BIDS import is still running.
  const [checked, setChecked] = useState<string[]>([]);
  const seen = useRef(new Set<string>());
  useEffect(() => {
    const fresh = subjects.filter(
      (s) => s.records.length && !seen.current.has(s.id),
    );
    for (const s of fresh) seen.current.add(s.id);
    if (fresh.length)
      setChecked((c) => [
        ...new Set([
          ...c,
          ...fresh.filter((s) => s.state.kind !== "completed").map((s) => s.id),
        ]),
      ]);
  }, [subjects.map((s) => `${s.id}:${s.records.length}`).join()]);
  const selected = subjects.filter(
    (s) => checked.includes(s.id) && available(s),
  );
  const recordings = selected.reduce((n, s) => n + s.records.length, 0);
  const batch = pipeline.batch;
  const finished = batch.filter((id) => !pending.some((j) => j.id === id));
  const results = pipeline.jobs.filter((j) => batch.includes(j.id));
  const failed = results.filter((j) => j.status !== "completed");
  const named = (id: string) =>
    pipeline.recordings.find((r) => r.id === id) as Recording;
  return (
    <>
      {!!pending.length && !!batch.length && (
        <div className="batch-progress" role="status">
          <div>
            <strong>
              <LoaderCircle size={15} className="spin" /> Processed{" "}
              {finished.length} of {plural(batch.length, "job")}
            </strong>
            <button
              className="button"
              disabled={busy}
              onClick={() =>
                void act(async () =>
                  onPipeline(await window.eyeris.cancelPipeline()),
                )
              }
            >
              <Square size={13} /> Cancel all
            </button>
          </div>
          <div
            className="progress-track"
            role="progressbar"
            aria-label="Batch progress"
            aria-valuemin={0}
            aria-valuemax={batch.length}
            aria-valuenow={finished.length}
          >
            <span
              style={{
                width: `${(100 * finished.length) / batch.length}%`,
              }}
            />
          </div>
          <ul>
            {pipeline.running.map((j) => {
              const current = named(j.current);
              return (
                <li key={j.id}>
                  sub-{current?.subject} · {current && bidsName(current)} ·{" "}
                  {j.phase === "starting" ? "starting R" : j.phase}
                </li>
              );
            })}
            {!!pipeline.queued.length && (
              <li>{plural(pipeline.queued.length, "job")} waiting</li>
            )}
          </ul>
        </div>
      )}
      {!pending.length && !!results.length && (
        <div className="batch-result" role="status">
          <strong className={failed.length ? "failed" : "completed"}>
            Batch finished: {results.length - failed.length} of{" "}
            {plural(results.length, "job")} completed
          </strong>
          {!!failed.length && (
            <ul className="batch-failures">
              {failed.map((j) => {
                const subject = named(j.recordings[0])?.subject;
                return (
                  <li key={j.id}>
                    <button onClick={() => onSubject(subject)}>
                      sub-{subject}
                    </button>{" "}
                    {j.status}
                  </li>
                );
              })}
            </ul>
          )}
          {results.some((j) => j.status === "completed" && j.config.epoch) && (
            <button className="button primary" onClick={onReview}>
              Review epochs
            </button>
          )}
        </div>
      )}
      <section className="recordings-section">
        <div className="section-heading">
          <h2>Subjects</h2>
          <small>
            Each subject is processed as its own job, with its runs in order.
          </small>
        </div>
        {subjects.length ? (
          <table className="recordings-table batch-table">
            <thead>
              <tr>
                <th className="select-column">
                  <input
                    type="checkbox"
                    aria-label="Select all subjects"
                    checked={
                      !!selected.length &&
                      selected.length === subjects.filter(available).length
                    }
                    ref={(el) => {
                      if (el)
                        el.indeterminate =
                          !!selected.length &&
                          selected.length < subjects.filter(available).length;
                    }}
                    onChange={(e) =>
                      setChecked(
                        e.target.checked
                          ? subjects.filter(available).map((s) => s.id)
                          : [],
                      )
                    }
                  />
                </th>
                <th>Subject</th>
                <th>Recordings</th>
                <th>Status</th>
              </tr>
            </thead>
            <tbody>
              {subjects.map((s) => (
                <tr key={s.id}>
                  <td className="select-column">
                    <input
                      type="checkbox"
                      aria-label={`Process sub-${s.id}`}
                      disabled={!available(s)}
                      checked={checked.includes(s.id) && available(s)}
                      onChange={(e) =>
                        setChecked((c) =>
                          e.target.checked
                            ? [...c, s.id]
                            : c.filter((id) => id !== s.id),
                        )
                      }
                    />
                  </td>
                  <td>
                    <button onClick={() => onSubject(s.id)}>sub-{s.id}</button>
                  </td>
                  <td>
                    {s.records.length
                      ? `${plural(s.records.length, "recording")} · ${[
                          ...new Set(
                            s.records.map(
                              (r) => `ses-${r.session} task-${r.task}`,
                            ),
                          ),
                        ].join(", ")}`
                      : "—"}
                  </td>
                  <td>
                    <span className={`job-status ${s.state.kind}`}>
                      {s.state.label}
                    </span>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        ) : (
          <p className="run-note">
            Import a BIDS folder or create subjects to process them together.
          </p>
        )}
      </section>
      {!!subjects.length && (
        <PipelineSettingsPanel
          settings={settings}
          setSettings={setSettings}
          disabled={busy}
          onError={onError}
          epochExtras={epochExtras}
        >
          <label className="parallel-field">
            Subjects at a time
            <select
              aria-label="Subjects at a time"
              value={String(pipeline.parallel.setting)}
              disabled={busy}
              onChange={(e) =>
                void act(async () =>
                  onPipeline(
                    await window.eyeris.setParallelJobs(
                      e.target.value === "auto"
                        ? "auto"
                        : Number(e.target.value),
                    ),
                  ),
                )
              }
            >
              <option value="auto">
                Automatic ({pipeline.parallel.automatic})
              </option>
              {Array.from({ length: pipeline.parallel.cores }, (_, i) => (
                <option key={i + 1} value={i + 1}>
                  {i + 1}
                </option>
              ))}
            </select>
          </label>
          <div className="run-actions">
            <button
              className="button primary"
              disabled={
                busy ||
                !!pipeline.importing ||
                !selected.length ||
                (!!settings.epoch && !settings.epoch.events.trim())
              }
              onClick={() =>
                void act(async () => {
                  onPipeline(
                    await window.eyeris.queuePipeline(
                      selected.map((s) => s.records.map((r) => r.id)),
                      settings,
                    ),
                  );
                  // Processed subjects are not selected again by default.
                  setChecked((c) =>
                    c.filter((id) => !selected.some((s) => s.id === id)),
                  );
                })
              }
            >
              <Play size={15} />{" "}
              {selected.length
                ? `Process ${plural(selected.length, "subject")}`
                : "Process subjects"}
            </button>
          </div>
          <p className="run-note">
            {selected.length
              ? `${plural(recordings, "recording")} in ${plural(selected.length, "job")}, processed ${
                  pipeline.parallel.jobs === 1
                    ? "one subject at a time"
                    : `up to ${pipeline.parallel.jobs} subjects at a time in separate R processes`
                }. Each subject's runs stay in order in one process.`
              : "Select the subjects to process."}
            {!!pending.length &&
              " Setting changes apply to jobs started later."}
          </p>
        </PipelineSettingsPanel>
      )}
    </>
  );
}
