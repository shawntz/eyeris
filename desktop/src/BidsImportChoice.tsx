import { useState } from "react";
import { FolderInput } from "lucide-react";
import type { BidsGroup, BidsGroupKey, BidsPreview, Recording } from "./types";

const key = (g: BidsGroupKey) => `${g.session}/${g.task}`;
const name = (g: BidsGroupKey) => `ses-${g.session} · task-${g.task}`;
const plural = (n: number, word: string) => `${n} ${word}${n === 1 ? "" : "s"}`;
const size = (n: number) =>
  n < 1e9
    ? `${(n / 1e6).toFixed(n < 1e7 ? 1 : 0)} MB`
    : `${(n / 1e9).toFixed(1)} GB`;

// Choose the sessions and tasks of a BIDS dataset to import. One set of
// pipeline and epoch settings applies to a whole project, so sessions or tasks
// with different event messages belong in separate projects. Those the project
// already has are chosen to begin with.
export function BidsImportChoice({
  preview,
  recordings,
  disabled,
  onImport,
  onCancel,
}: {
  preview: BidsPreview;
  recordings: Recording[];
  disabled: boolean;
  onImport: (groups: BidsGroup[]) => void;
  onCancel: () => void;
}) {
  const [chosen, setChosen] = useState(() =>
    preview.groups.filter((g) => g.inProject).map(key),
  );
  const groups = preview.groups.filter((g) => chosen.includes(key(g)));
  const files = groups.reduce((n, g) => n + g.files, 0);
  // Sessions and tasks the project has that the new ones would join.
  const existing = [
    ...new Map(recordings.map((r) => [key(r), r] as const)).values(),
  ];
  const joining = groups.filter((g) => !g.inProject);
  return (
    <section className="import-choice" aria-labelledby="import-choice-title">
      <div className="import-choice-heading">
        <strong id="import-choice-title">
          <FolderInput size={15} /> Choose what to import
        </strong>
        <small>{preview.root}</small>
      </div>
      <p>
        This dataset has recordings for {preview.groups.length} combinations of
        session and task. Pipeline and epoch settings apply to the whole
        project, so sessions or tasks with different event messages are best
        imported into separate projects.
      </p>
      <ul>
        {preview.groups.map((g) => (
          <li key={key(g)}>
            <label>
              <input
                type="checkbox"
                checked={chosen.includes(key(g))}
                disabled={disabled}
                onChange={(e) =>
                  setChosen((c) =>
                    e.target.checked
                      ? [...c, key(g)]
                      : c.filter((k) => k !== key(g)),
                  )
                }
              />
              <span>{name(g)}</span>
              <small>
                {plural(g.subjects, "subject")} · {plural(g.files, "file")} ·{" "}
                {size(g.bytes)}
                {!!g.inProject && ` · ${g.inProject} already in this project`}
              </small>
            </label>
          </li>
        ))}
      </ul>
      {!!existing.length && !!joining.length && (
        <p className="import-choice-warning" role="note">
          This project already has {existing.map(name).join(", ")}.{" "}
          {joining.map(name).join(", ")} would be processed with the same
          settings.
        </p>
      )}
      <div className="import-choice-actions">
        <button className="button" disabled={disabled} onClick={onCancel}>
          Cancel
        </button>
        <button
          className="button primary"
          disabled={disabled || !groups.length}
          onClick={() => onImport(groups)}
        >
          {groups.length
            ? `Import ${plural(files, "file")}`
            : "Choose a session and task"}
        </button>
      </div>
    </section>
  );
}
