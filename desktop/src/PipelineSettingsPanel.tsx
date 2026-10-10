import {
  useState,
  type Dispatch,
  type ReactNode,
  type SetStateAction,
} from "react";
import { ChevronDown, Check } from "lucide-react";
import type { PipelineSettings } from "./types";
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
export const defaults: PipelineSettings = {
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

// The glassbox, epoch and output settings shared by every subject and run.
export function PipelineSettingsPanel({
  settings,
  setSettings,
  disabled,
  onError,
  children,
}: {
  settings: PipelineSettings;
  setSettings: Dispatch<SetStateAction<PipelineSettings>>;
  disabled: boolean;
  onError: (message: string) => void;
  children: ReactNode;
}) {
  const [openStep, setOpenStep] = useState("");
  function setStep(key: string, value: PipelineSettings["glassbox"][string]) {
    setSettings((s) => ({ ...s, glassbox: { ...s.glassbox, [key]: value } }));
  }
  function epoch(change: Partial<NonNullable<PipelineSettings["epoch"]>>) {
    setSettings((s) => ({ ...s, epoch: { ...s.epoch!, ...change } }));
  }
  return (
    <div className="pipeline-layout">
      <section className="pipeline-config">
        <div className="section-heading">
          <h2>Glassbox pipeline</h2>
          <button
            onClick={() => setSettings(structuredClone(defaults))}
            disabled={disabled}
          >
            Reset defaults
          </button>
        </div>
        <fieldset disabled={disabled}>
          <div className="load-options">
            <label>
              Eye
              <select
                aria-label="Eye"
                value={String(
                  (settings.glassbox.load_asc as Record<string, unknown>)
                    .binocular_mode,
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
                onChange={(e) => setStep("seed", Number(e.target.value))}
              />
            </label>
          </div>
          <div className="pipeline-steps">
            {steps.map((s, i) => {
              const enabled = !!settings.glassbox[s.key];
              const params: Record<string, unknown> =
                typeof settings.glassbox[s.key] === "object"
                  ? (settings.glassbox[s.key] as Record<string, unknown>)
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
                          setOpenStep(openStep === s.key ? "" : s.key)
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
                                value={value === null ? "" : Number(value)}
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
              Override supported step parameters using the package’s argument
              names.
            </p>
            <textarea
              aria-label="Advanced glassbox options"
              key={JSON.stringify(settings.glassbox)}
              defaultValue={JSON.stringify(settings.glassbox, null, 2)}
              onBlur={(e) => {
                try {
                  const parsed = JSON.parse(e.target.value);
                  if (!parsed.load_asc || typeof parsed.load_asc !== "object")
                    throw new Error();
                  setSettings((s) => ({ ...s, glassbox: parsed }));
                  onError("");
                } catch {
                  onError(
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
        <fieldset disabled={disabled}>
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
                  onChange={(e) => epoch({ events: e.target.value })}
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
                      onChange={(e) => epoch({ baseline_type: e.target.value })}
                    >
                      <option value="sub">Subtract baseline</option>
                      <option value="div">Divide by baseline</option>
                    </select>
                  </label>
                  <div className="paired-fields">
                    <label>
                      Baseline start (s)
                      <input
                        type="number"
                        step="any"
                        value={settings.epoch.baseline_period?.[0] ?? -1}
                        onChange={(e) =>
                          epoch({
                            baseline_period: [
                              Number(e.target.value),
                              settings.epoch!.baseline_period?.[1] ?? 0,
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
                        value={settings.epoch.baseline_period?.[1] ?? 0}
                        onChange={(e) =>
                          epoch({
                            baseline_period: [
                              settings.epoch!.baseline_period?.[0] ?? -1,
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
        {children}
      </section>
    </div>
  );
}
