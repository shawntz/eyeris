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
  limits: number[] | null;
}
export interface Epoch {
  id: string;
  source_id: string;
  participant: string;
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
  stages: string[];
  canUndo: boolean;
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
export interface PipelineSettings {
  glassbox: Record<string, boolean | number | Record<string, unknown>>;
  epoch: {
    events: string;
    limits: [number, number] | null;
    label: string;
    baseline: boolean;
    baseline_type?: string;
    baseline_period?: number[];
    baseline_events?: string;
  } | null;
  report: boolean;
  database: boolean;
}
export interface Recording {
  id: string;
  subject: string;
  session: string;
  task: string;
  name: string;
  file: string;
}
export interface Job {
  id: string;
  recording_id: string;
  status: string;
  phase: string;
  config: PipelineSettings;
  started_at: string;
  finished_at: string | null;
  error: string | null;
  outputs: string;
}
export interface PipelineState {
  subjects: { id: string }[];
  recordings: Recording[];
  jobs: Job[];
  active: { id: string; phase: string; log: string } | null;
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
  }): Promise<PipelineState | null>;
  startPipeline(id: string, settings: PipelineSettings): Promise<PipelineState>;
  cancelPipeline(): Promise<PipelineState>;
  pipelineLog(id: string): Promise<string>;
  showProjectFiles(kind: string, id?: string): Promise<void>;
  demo(): Promise<Summary>;
  importFiles(): Promise<{
    project: Summary;
    results: {
      file: string;
      count?: number;
      duplicate?: boolean;
      error?: string;
    }[];
  } | null>;
  list(filters: Filters): Promise<Queue>;
  trace(id: string, stage: string, range?: [number, number]): Promise<Trace>;
  decide(input: {
    id: string;
    status: Status;
    stage: string;
    reason: string;
    reviewer: string;
  }): Promise<{ epoch: Epoch; project: Summary }>;
  undo(): Promise<{ epoch: Epoch | null; project: Summary }>;
  exportData(): Promise<{
    directory: string;
    counts: Summary["counts"];
  } | null>;
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
