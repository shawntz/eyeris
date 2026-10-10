import { useState } from "react";
import { Trash2, X } from "lucide-react";
import type { PipelineState, SessionRemoval, Summary } from "./types";

const plural = (n: number, word: string) => `${n} ${word}${n === 1 ? "" : "s"}`;
let dismissedRemoval = "";

// The sessions in the project, each of which can be removed: one set of
// pipeline and epoch settings applies to the whole project, so a session with
// different event messages belongs in a project of its own.
export function ProjectSessions({
  pipeline,
  disabled,
  act,
  onRemoved,
}: {
  pipeline: PipelineState;
  disabled: boolean;
  act: (fn: () => Promise<void>) => Promise<void>;
  onRemoved: (r: { project: Summary; pipeline: PipelineState }) => void;
}) {
  const [confirming, setConfirming] = useState<SessionRemoval | null>(null);
  const [dismissed, setDismissed] = useState(dismissedRemoval);
  const sessions = [...new Set(pipeline.recordings.map((r) => r.session))].map(
    (session) => {
      const records = pipeline.recordings.filter((r) => r.session === session);
      return {
        session,
        recordings: records.length,
        subjects: new Set(records.map((r) => r.subject)).size,
        tasks: [...new Set(records.map((r) => r.task))],
      };
    },
  );
  const removed =
    pipeline.lastRemoval?.id !== dismissed ? pipeline.lastRemoval : null;
  if (sessions.length < 2 && !removed) return null;
  const busy =
    disabled ||
    !!pipeline.running.length ||
    !!pipeline.queued.length ||
    !!pipeline.importing;
  return (
    <section className="recordings-section project-sessions">
      <div className="section-heading">
        <h2>Sessions</h2>
        <small>
          Sessions with different event messages are best processed in separate
          projects.
        </small>
      </div>
      {removed && (
        <div
          className={`message ${removed.leftover.length ? "error" : "notice"}`}
          role="status"
        >
          <div>
            <strong>
              Removed ses-{removed.session}:{" "}
              {plural(removed.recordings, "recording")}
              {removed.subjects
                ? ` and ${plural(removed.subjects, "subject")} without other recordings`
                : ""}
              .
            </strong>
            {!!removed.leftover.length && (
              <p>
                These files could not be deleted and can be removed by hand:{" "}
                {removed.leftover.join(", ")}
              </p>
            )}
          </div>
          <button
            aria-label="Dismiss removal summary"
            onClick={() => {
              dismissedRemoval = removed.id;
              setDismissed(removed.id);
            }}
          >
            <X size={15} />
          </button>
        </div>
      )}
      {sessions.length > 1 && (
        <ul className="session-list">
          {sessions.map((s) => (
            <li key={s.session}>
              <div className="session-row">
                <strong>ses-{s.session}</strong>
                <span>
                  {plural(s.subjects, "subject")} ·{" "}
                  {plural(s.recordings, "recording")} ·{" "}
                  {s.tasks.map((t) => `task-${t}`).join(", ")}
                </span>
                <button
                  className="button"
                  disabled={busy}
                  onClick={() =>
                    void act(async () =>
                      setConfirming(
                        await window.eyeris.sessionRemoval(s.session),
                      ),
                    )
                  }
                >
                  <Trash2 size={14} /> Remove ses-{s.session}…
                </button>
              </div>
              {confirming?.session === s.session && (
                <div
                  className="session-confirm"
                  role="alertdialog"
                  aria-label={`Remove ses-${s.session}`}
                >
                  <strong>Remove ses-{s.session} from this project?</strong>
                  <ul>
                    <li>
                      {plural(confirming.recordings, "recording")} of{" "}
                      {plural(confirming.subjects, "subject")}, with their
                      copies in the project
                    </li>
                    {!!confirming.jobs && (
                      <li>
                        {plural(confirming.jobs, "processing run")} and their
                        published BIDS derivatives
                      </li>
                    )}
                    {!!confirming.epochs && (
                      <li>
                        {plural(confirming.epochs, "epoch")} in review
                        {confirming.reviewed
                          ? `, ${confirming.reviewed} of them already kept or excluded`
                          : ""}
                      </li>
                    )}
                    {!!confirming.emptied.length && (
                      <li>
                        {confirming.emptied.map((id) => `sub-${id}`).join(", ")}
                        , with no other recordings
                      </li>
                    )}
                  </ul>
                  <p>
                    The original .asc files are not touched, so the session can
                    be imported into another project. This cannot be undone.
                  </p>
                  <div className="session-confirm-actions">
                    <button
                      className="button"
                      disabled={disabled}
                      onClick={() => setConfirming(null)}
                    >
                      Cancel
                    </button>
                    <button
                      className="button primary"
                      disabled={busy}
                      onClick={() =>
                        void act(async () => {
                          onRemoved(
                            await window.eyeris.removeSession(s.session),
                          );
                          setConfirming(null);
                        })
                      }
                    >
                      Remove ses-{s.session}
                    </button>
                  </div>
                </div>
              )}
            </li>
          ))}
        </ul>
      )}
    </section>
  );
}
