import { useEffect, useState } from "react";
import { stageName, type Summary } from "./types";

// The project's rules for excluding epochs with too much missing data, or with
// missing data at their start or end, and for keeping epochs with none. They
// apply to every epoch without a person's decision, now and on import.
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
  const [edgeWindow, setEdgeWindow] = useState(String(saved.edgeWindow));
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState("");
  useEffect(() => {
    setRule(saved);
    setThreshold(String(saved.threshold));
    setEdgeWindow(String(saved.edgeWindow));
  }, [
    saved.enabled,
    saved.threshold,
    saved.stage,
    saved.edges,
    saved.edgeWindow,
    saved.edgeStage,
    saved.keepComplete,
    saved.keepStage,
  ]);
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
      setEdgeWindow(String(saved.edgeWindow));
    } finally {
      setSaving(false);
    }
  }
  // Each rule is measured at its own stage.
  const stageSelect = (
    label: string,
    key: "stage" | "edgeStage" | "keepStage",
    on: boolean,
  ) => (
    <label className="stage-field">
      Measured at
      <select
        aria-label={label}
        value={rule[key]}
        disabled={disabled || saving || !on}
        onChange={(e) => void save({ [key]: e.target.value })}
      >
        <option value="final">Final available stage</option>
        {[...new Set([...project.stages, rule[key]])]
          .filter((s) => s !== "final")
          .map((s) => (
            <option key={s} value={s}>
              {stageName(s)} · {s.replace(/^pupil_/, "")}
            </option>
          ))}
      </select>
    </label>
  );
  return (
    <div className="auto-exclude">
      <div className="auto-rule">
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
          {stageSelect("Missing data stage", "stage", rule.enabled)}
        </div>
      </div>
      <div className="auto-rule">
        <label className="check-label">
          <input
            type="checkbox"
            checked={rule.edges}
            disabled={disabled || saving}
            onChange={(e) => void save({ edges: e.target.checked })}
          />{" "}
          Automatically exclude epochs missing data at the start or end
        </label>
        <div className="paired-fields">
          <label className="edge-window">
            Within (ms) of either end
            <input
              type="number"
              min={0}
              max={60000}
              step="any"
              aria-label="Edge window"
              value={edgeWindow}
              disabled={disabled || saving || !rule.edges}
              onChange={(e) => setEdgeWindow(e.target.value)}
              onBlur={() => {
                if (edgeWindow.trim() && Number(edgeWindow) !== rule.edgeWindow)
                  void save({ edgeWindow: Number(edgeWindow) });
                else setEdgeWindow(String(rule.edgeWindow));
              }}
              onKeyDown={(e) => {
                if (e.key === "Enter") e.currentTarget.blur();
              }}
            />
          </label>
          {stageSelect("Start and end stage", "edgeStage", rule.edges)}
          <small className="edge-note">
            {Number(edgeWindow) > 0
              ? `Any sample missing in the first or last ${edgeWindow} ms.`
              : "The first or last sample missing, such as a blink still under way."}
          </small>
        </div>
      </div>
      <div className="auto-rule">
        <label className="check-label">
          <input
            type="checkbox"
            checked={rule.keepComplete}
            disabled={disabled || saving}
            onChange={(e) => void save({ keepComplete: e.target.checked })}
          />{" "}
          Automatically keep epochs with no missing data
        </label>
        <div className="paired-fields keep-fields">
          {stageSelect("Complete data stage", "keepStage", rule.keepComplete)}
        </div>
      </div>
      <small>
        {saved.enabled || saved.edges
          ? `${project.autoExcluded.toLocaleString()} epoch${project.autoExcluded === 1 ? "" : "s"} excluded automatically. `
          : ""}
        {saved.keepComplete
          ? `${project.autoKept.toLocaleString()} epoch${project.autoKept === 1 ? "" : "s"} with no missing data kept automatically. `
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
