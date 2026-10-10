import { useEffect, useRef, useState } from "react";
import { FolderOpen, Plus, ArrowRight, X } from "lucide-react";
import type { Summary, PipelineState, PipelineSettings } from "./types";
import { defaults } from "./PipelineSettingsPanel";
import { ReviewWorkspace } from "./ReviewWorkspace";
import { ProjectWorkspace } from "./ProjectWorkspace";
import { DiagnosticsWorkspace } from "./DiagnosticsWorkspace";
const emptyPipeline: PipelineState = {
  subjects: [],
  recordings: [],
  jobs: [],
  running: [],
  queued: [],
  batch: [],
  settings: null,
  parallel: { setting: "auto", jobs: 1, cores: 1, automatic: 1 },
  importing: null,
  lastImport: null,
};
export function App() {
  const [project, setProject] = useState<Summary | null>(null);
  const [version, setVersion] = useState("");
  const [pipeline, setPipeline] = useState<PipelineState>(emptyPipeline);
  // Pipeline settings belong to the project and apply to every subject.
  const [settings, setSettings] = useState<PipelineSettings>(
    structuredClone(defaults),
  );
  const loadedSettings = useRef<PipelineSettings | null>(null);
  const [reviewer, setReviewer] = useState("");
  const [recent, setRecent] = useState<string | null>(null);
  const [screen, setScreen] = useState<"subjects" | "review" | "diagnostics">(
    "subjects",
  );
  const [diagnosticsKey, setDiagnosticsKey] = useState<string>();
  const showDiagnostics = (key?: string) => {
    setDiagnosticsKey(key);
    setScreen("diagnostics");
  };
  const showReview = (participant = "") => {
    setReviewParticipant(participant);
    setScreen("review");
  };
  const [reviewParticipant, setReviewParticipant] = useState("");
  const [error, setError] = useState("");
  const [runtimeReady, setRuntimeReady] = useState(false);
  const [busy, setBusy] = useState(false);
  useEffect(() => {
    window.eyeris
      .init()
      .then((r) => {
        setVersion(r.appVersion);
        setReviewer(r.reviewer);
        setRecent(r.recent);
        if (r.warning) setError(r.warning);
        else setRuntimeReady(true);
      })
      .catch((e) => setError(e.message));
  }, []);
  useEffect(() => {
    if (!project) return;
    let alive = true,
      pending = false;
    const update = async () => {
      if (pending) return;
      pending = true;
      try {
        const r = await window.eyeris.projectState();
        if (alive) {
          setProject(r.project);
          setPipeline(r.pipeline);
        }
      } catch (e) {
        if (alive) setError(String(e));
      } finally {
        pending = false;
      }
    };
    void update();
    const timer = setInterval(update, 1200);
    return () => {
      alive = false;
      clearInterval(timer);
    };
  }, [project?.directory]);
  async function open(method: "createProject" | "openProject" | "openRecent") {
    setBusy(true);
    setError("");
    try {
      const r = await window.eyeris[method]();
      if (r) {
        const state = await window.eyeris.projectState();
        setPipeline(state.pipeline);
        loadedSettings.current = structuredClone(
          state.pipeline.settings ?? defaults,
        );
        setSettings(loadedSettings.current);
        setProject(r);
        setRecent(r.directory);
        setScreen("subjects");
      }
    } catch (e) {
      setError(String(e));
    } finally {
      setBusy(false);
    }
  }
  // Save edits shortly after they are made, and before the project closes.
  const unsaved = useRef<PipelineSettings | null>(null);
  async function saveSettings() {
    const pending = unsaved.current;
    unsaved.current = null;
    if (pending) setPipeline(await window.eyeris.saveSettings(pending));
  }
  useEffect(() => {
    if (!project || settings === loadedSettings.current) return;
    unsaved.current = settings;
    const timer = setTimeout(
      () => void saveSettings().catch((e) => setError(String(e))),
      400,
    );
    return () => clearTimeout(timer);
  }, [settings]);
  async function close() {
    try {
      await saveSettings();
      await window.eyeris.closeProject();
      setProject(null);
      setPipeline(emptyPipeline);
      setError("");
    } catch (e) {
      setError(String(e));
    }
  }
  if (!project)
    return (
      <div className="splash">
        <header>
          <span>eyeris Desktop</span>
          <span>{version}</span>
        </header>
        <section className="splash-content">
          <img
            className="splash-sticker"
            src="./sticker.png"
            alt="eyeris logo sticker"
          />
          <div className="splash-body">
            <h1>eyeris</h1>
            <p>Pupillometry preprocessing and epoch review.</p>
            <div className="splash-actions">
              <button
                className="button primary"
                disabled={busy || !runtimeReady}
                onClick={() => void open("createProject")}
              >
                <Plus size={17} /> New project
              </button>
              <button
                className="button"
                disabled={busy || !runtimeReady}
                onClick={() => void open("openProject")}
              >
                <FolderOpen size={17} /> Open project
              </button>
            </div>
            {recent && (
              <div className="recent-project">
                <h2>Recent project</h2>
                <button
                  disabled={busy || !runtimeReady}
                  onClick={() => void open("openRecent")}
                >
                  <span>
                    {recent
                      .split(/[\\/]/)
                      .at(-1)
                      ?.replace(/\.eyeris$/, "")}
                    <small>{recent}</small>
                  </span>
                  <ArrowRight size={17} />
                </button>
              </div>
            )}
            {!runtimeReady && !error && (
              <p role="status">Checking processing tools…</p>
            )}
            {error && (
              <div className="message error" role="alert">
                {error}
              </div>
            )}
          </div>
        </section>
        <footer>
          Projects keep source recordings, processing outputs, and review
          decisions together.
        </footer>
      </div>
    );
  return (
    <>
      {error && (
        <div className="app-error" role="alert">
          {error}
          <button aria-label="Dismiss error" onClick={() => setError("")}>
            <X size={16} />
          </button>
        </div>
      )}
      {screen === "diagnostics" ? (
        <DiagnosticsWorkspace
          project={project}
          initialKey={diagnosticsKey}
          onSubjects={() => setScreen("subjects")}
          onReview={showReview}
          onClose={() => void close()}
        />
      ) : screen === "review" ? (
        <ReviewWorkspace
          initialProject={project}
          reviewer={reviewer}
          participant={reviewParticipant}
          onSubjects={() => setScreen("subjects")}
          onDiagnostics={showDiagnostics}
          onClose={() => void close()}
        />
      ) : (
        <ProjectWorkspace
          project={project}
          pipeline={pipeline}
          settings={settings}
          setSettings={setSettings}
          onPipeline={setPipeline}
          onReview={showReview}
          onDiagnostics={() => showDiagnostics()}
          onClose={() => void close()}
          onProject={setProject}
        />
      )}
    </>
  );
}
