export type Status = "unreviewed" | "keep" | "exclude";
export interface EpochMeta {
  key: string;
  eye: string;
  label: string;
  block: string;
  start: number;
  end: number;
  ordinal: number;
  trial: string;
  event: string;
  stages: string[];
  finalStage: string;
  samples: number;
  duration: number;
  missing: number;
  stageMissing?: Record<string, number>;
  blocks?: number | null;
  limits: number[] | null;
}
export interface Epoch {
  id: string;
  source_id: string;
  participant: string;
  session: string;
  task: string;
  run: string;
  event: string;
  trial: string;
  label: string;
  block: string;
  eye: string;
  ordinal: number;
  missing: number;
  meta: EpochMeta;
  status: Status;
  reason: string;
  reviewer: string;
  stage: string;
  updated_at: string | null;
}
export interface Summary {
  name: string;
  directory: string;
  counts: { total: number; keep: number; exclude: number; unreviewed: number };
  sources: { id: string; name: string; participant: string }[];
  participants: string[];
  labels: string[];
  runs: string[];
  stages: string[];
  autoExclude: AutoExclude;
  autoExcluded: number;
  // Review progress for each participant.
  progress: {
    participant: string;
    total: number;
    keep: number;
    exclude: number;
    unreviewed: number;
  }[];
  // Where review was last left.
  position: { epochId: string | null; filters: Filters } | null;
  exporting: { total: number; completed: number; current: string } | null;
  lastExport: {
    id: string;
    directory: string | null;
    counts: Summary["counts"] | null;
    error: string | null;
  } | null;
  canUndo: boolean;
}
// Exclude unreviewed epochs missing more than `threshold` percent of samples.
export interface AutoExclude {
  enabled: boolean;
  threshold: number;
  stage: string;
}
export const AUTO_REVIEWER = "eyeris auto-exclude";
// A run of epochs that share a time axis: one block, label and eye of a source.
export interface DiagnosticGroup {
  key: string;
  source_id: string;
  source_name: string;
  participant: string;
  session: string;
  task: string;
  run: string;
  label: string;
  eye: string;
  block: string;
  epochs: number;
  keep: number;
  exclude: number;
  unreviewed: number;
}
export type Include = "all" | "included" | "kept";
export interface Average {
  epochs: number;
  stage: string;
  // Time from the event when the epoch window is known, else from its start.
  onset?: boolean;
  time?: number[];
  traces?: (number | null)[][];
  mean?: (number | null)[];
  se?: (number | null)[];
  n?: number[];
  ids?: string[];
}
export interface Behavior {
  root: string | null;
  bidsRoot: string | null;
  files: number;
  rows: number;
  columns: string[];
}
export interface SplitRequest {
  key: string;
  scope: "run" | "subject" | "all";
  stage: string;
  include: Include;
  by: { from: "epoch" | "behavior"; column: string } | null;
  join: { epoch: string; behavior: string } | null;
}
export interface Split {
  epochs: number;
  total: number;
  unmatched: number;
  ambiguous: number;
  missing: number;
  stage: string;
  onset?: boolean;
  time?: number[];
  series: {
    label: string;
    n: number;
    mean: (number | null)[];
    se: (number | null)[];
  }[];
}
export interface Trace {
  time: number[];
  signal: (number | null)[];
  domain: [number, number];
  samples: number;
  displayed: number;
  missing: number;
  stage: string;
}
export interface Filters {
  status: Status | "all";
  participant: string;
  run: string;
  // An epoch segment's label, such as epoch_poststim.
  label: string;
  search: string;
  stage: string;
  sort: string;
  offset: number;
}
export interface Queue {
  rows: Epoch[];
  total: number;
  offset: number;
}
// One epoch segment, such as a prestimulus or poststimulus window.
export interface EpochSetting {
  events: string;
  limits: [number, number] | null;
  label: string;
  baseline: boolean;
  baseline_type?: string;
  baseline_period?: number[];
  baseline_events?: string;
}
export interface PipelineSettings {
  glassbox: Record<string, boolean | number | Record<string, unknown>>;
  // Every segment is cut from the same preprocessed recording. Empty when
  // epochs are not extracted.
  epochs: EpochSetting[];
  // Settings saved before several segments were supported.
  epoch?: EpochSetting | null;
  report: boolean;
  database: boolean;
}
// Epoch segments of settings, including those saved with a single epoch.
export function epochsOf(settings: PipelineSettings): EpochSetting[] {
  if (Array.isArray(settings.epochs)) return settings.epochs;
  return settings.epoch ? [settings.epoch] : [];
}
// Why the epoch segments cannot run yet, or "" when they can.
export function epochProblem(settings: PipelineSettings) {
  const epochs = epochsOf(settings);
  const labels = epochs.map((e) => e.label.trim().toLowerCase());
  if (epochs.some((e) => !e.events.trim()))
    return "Enter an event pattern for every epoch segment.";
  if (epochs.some((e) => e.baseline && !e.baseline_events?.trim()))
    return "Enter the baseline's event pattern.";
  if (labels.some((l) => !l)) return "Give every epoch segment a label.";
  if (epochs.some((e) => e.limits && !(e.limits[0] < e.limits[1])))
    return "Epoch start must be earlier than epoch end.";
  if (new Set(labels).size !== labels.length)
    return "Give each epoch segment a different label.";
  return "";
}
export function normalizeSettings(
  settings: PipelineSettings,
): PipelineSettings {
  const { epoch: _, ...rest } = settings;
  return { ...rest, epochs: epochsOf(settings) };
}
export interface Recording {
  id: string;
  subject: string;
  session: string;
  task: string;
  run: string;
  name: string;
  file: string;
}
export interface Job {
  id: string;
  recording_id: string;
  recordings: string[];
  status: string;
  phase: string;
  config: PipelineSettings;
  started_at: string;
  finished_at: string | null;
  error: string | null;
  outputs: string;
}
export interface RunningJob {
  id: string;
  phase: string;
  log: string;
  recordings: string[];
  current: string;
}
export interface PipelineState {
  subjects: { id: string }[];
  recordings: Recording[];
  jobs: Job[];
  running: RunningJob[];
  queued: { id: string; recordings: string[] }[];
  // Jobs queued since processing was last idle.
  batch: string[];
  settings: PipelineSettings | null;
  // Subjects processed at once, each in its own R process.
  parallel: {
    setting: "auto" | number;
    jobs: number;
    cores: number;
    automatic: number;
  };
  importing: {
    total: number;
    completed: number;
    bytes: number;
    totalBytes: number;
    current: string;
  } | null;
  lastImport: {
    id: string;
    root: string;
    added: number;
    subjects: number;
    skipped: { file: string; reason: string }[];
    cancelled: boolean;
    error: string | null;
  } | null;
}
export interface UpdateState {
  status:
    | "unavailable"
    | "idle"
    | "checking"
    | "current"
    | "available"
    | "downloading"
    | "downloaded"
    | "installing"
    | "error";
  currentVersion: string;
  version?: string;
  percent?: number;
  message?: string;
}
interface API {
  updateState(): Promise<UpdateState>;
  checkForUpdates(): Promise<UpdateState>;
  downloadUpdate(): Promise<UpdateState>;
  installUpdate(): Promise<UpdateState>;
  init(): Promise<{
    appVersion: string;
    project: Summary | null;
    reviewer: string;
    warning: string;
    recent: string | null;
  }>;
  createProject(): Promise<Summary | null>;
  openProject(): Promise<Summary | null>;
  openRecent(): Promise<Summary>;
  closeProject(): Promise<void>;
  projectState(): Promise<{ project: Summary; pipeline: PipelineState }>;
  addSubject(id: string): Promise<PipelineState>;
  addRecording(input: {
    subject: string;
    session: string;
    task: string;
    run: string;
  }): Promise<PipelineState | null>;
  importBids(): Promise<PipelineState | null>;
  cancelImport(): Promise<PipelineState>;
  startPipeline(
    ids: string[],
    settings: PipelineSettings,
  ): Promise<PipelineState>;
  queuePipeline(
    groups: string[][],
    settings: PipelineSettings,
  ): Promise<PipelineState>;
  saveSettings(settings: PipelineSettings): Promise<PipelineState>;
  setParallelJobs(value: "auto" | number): Promise<PipelineState>;
  cancelPipeline(id?: string): Promise<PipelineState>;
  pipelineLog(id: string): Promise<string>;
  showProjectFiles(kind: string, id?: string): Promise<void>;
  demo(): Promise<Summary>;
  importFiles(): Promise<{
    project: Summary;
    results: {
      file: string;
      count?: number;
      autoExcluded?: number;
      duplicate?: boolean;
      error?: string;
    }[];
  } | null>;
  setAutoExclude(rule: AutoExclude): Promise<Summary>;
  list(filters: Filters): Promise<Queue>;
  diagnosticGroups(): Promise<DiagnosticGroup[]>;
  behavior(): Promise<Behavior>;
  linkBehavior(): Promise<Behavior | null>;
  epochFields(): Promise<string[]>;
  split(request: SplitRequest): Promise<Split>;
  average(selection: {
    key: string;
    stage: string;
    include: Include;
  }): Promise<Average>;
  trace(id: string, stage: string, range?: [number, number]): Promise<Trace>;
  decide(input: {
    id: string;
    status: Status;
    stage: string;
    reason: string;
    reviewer: string;
  }): Promise<{ epoch: Epoch; project: Summary }>;
  undo(): Promise<{ epoch: Epoch | null; project: Summary }>;
  // Starts a background export; progress is in the project summary.
  exportData(): Promise<Summary | null>;
  nextUnreviewed(
    filters: Filters,
    fromId: string | null,
  ): Promise<{ id: string; offset: number } | null>;
  saveReviewPosition(position: {
    epochId: string | null;
    filters: Filters;
  }): Promise<void>;
}
declare global {
  interface Window {
    eyeris: API;
  }
}
export function stageName(stage: string) {
  if (stage === "final") return "Final available stage";
  const suffix =
    stage
      .replace(/^pupil_/, "")
      .split("_")
      .at(-1) || stage;
  return (
    (
      {
        raw: "Raw signal",
        deblink: "Deblinked",
        interpolate: "Interpolated",
        detransient: "Transient removal",
        lpfilt: "Low-pass filtered",
        detrend: "Detrended",
        z: "Z-scored",
        zscore: "Z-scored",
        downsample: "Downsampled",
        bin: "Binned",
        resample: "Resampled",
        sub: "Baseline subtracted",
        div: "Baseline divided",
      } as Record<string, string>
    )[suffix] || suffix
  );
}
