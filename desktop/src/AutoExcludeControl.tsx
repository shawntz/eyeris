import { useEffect, useState } from "react";
import { stageName, type Summary } from "./types";

// The project's rule for excluding epochs with too much missing data. It is
// applied to every epoch without a person's decision, now and on import.
export function AutoExcludeControl({
  project,
  onProject,
  disabled = false,
}: {
  project: Summary;
  onProject: (project: Summary) => void;
  disabled?: boolean;
}) {
  const saved = project.autoExclude;
  // Show each change at once; the project's rule follows when it is saved.
  const [rule, setRule] = useState(saved);
  const [threshold, setThreshold] = useState(String(saved.threshold));
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState("");
  useEffect(() => {
    setRule(saved);
    setThreshold(String(saved.threshold));
  }, [saved.enabled, saved.threshold, saved.stage]);
  async function save(change: Partial<typeof rule>) {
    const next = { ...rule, ...change };
    setRule(next);
    setSaving(true);
    setError("");
    try {
      onProject(await window.eyeris.setAutoExclude(next));
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e));
      setRule(saved);
      setThreshold(String(saved.threshold));
    } finally {
      setSaving(false);
    }
  }
  const stages = [...new Set([...project.stages, rule.stage])].filter(
    (s) => s !== "final",
  );
  return (
    <div className="auto-exclude">
      <label className="check-label">
        <input
          type="checkbox"
          checked={rule.enabled}
          disabled={disabled || saving}
          onChange={(e) => void save({ enabled: e.target.checked })}
        />{" "}
        Automatically exclude epochs with missing data
      </label>
      <div className="paired-fields">
        <label>
          Missing more than (%)
          <input
            type="number"
            min={0}
            max={99.9}
            step="any"
            aria-label="Missing data threshold"
            value={threshold}
            disabled={disabled || saving || !rule.enabled}
            onChange={(e) => setThreshold(e.target.value)}
            onBlur={() => {
              if (threshold.trim() && Number(threshold) !== rule.threshold)
                void save({ threshold: Number(threshold) });
              else setThreshold(String(rule.threshold));
            }}
            onKeyDown={(e) => {
              if (e.key === "Enter") e.currentTarget.blur();
            }}
          />
        </label>
        <label>
          Measured at
          <select
            aria-label="Missing data stage"
            value={rule.stage}
            disabled={disabled || saving || !rule.enabled}
            onChange={(e) => void save({ stage: e.target.value })}
          >
            <option value="final">Final available stage</option>
            {stages.map((s) => (
              <option key={s} value={s}>
                {stageName(s)} · {s.replace(/^pupil_/, "")}
              </option>
            ))}
          </select>
        </label>
      </div>
      <small>
        {saved.enabled
          ? `${project.autoExcluded.toLocaleString()} epoch${project.autoExcluded === 1 ? "" : "s"} excluded automatically. `
          : ""}
        Applies to epochs no one has reviewed, including epochs added later. A
        reviewer's decision is never changed.
      </small>
      {error && (
        <p className="auto-exclude-error" role="alert">
          {error}
        </p>
      )}
    </div>
  );
}
