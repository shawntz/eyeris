import { useEffect, useState } from "react";
import type { UpdateState } from "./types";

export function UpdateStatus() {
  const [state, setState] = useState<UpdateState | null>(null);
  const [error, setError] = useState("");
  useEffect(() => {
    let alive = true;
    const refresh = () =>
      window.eyeris
        .updateState()
        .then((next) => {
          if (alive) setState(next);
        })
        .catch(() => {});
    void refresh();
    const timer = setInterval(refresh, 2000);
    return () => {
      alive = false;
      clearInterval(timer);
    };
  }, []);
  if (!state || state.status === "unavailable") return null;
  const action = async (
    method: "checkForUpdates" | "downloadUpdate" | "installUpdate",
  ) => {
    setError("");
    try {
      setState(await window.eyeris[method]());
    } catch (e) {
      setError(String(e).replace(/^Error: /, ""));
    }
  };
  const text = {
    idle: `eyeris ${state.currentVersion}`,
    checking: "Checking for updates…",
    current: `eyeris ${state.currentVersion} is up to date`,
    available: `eyeris ${state.version} is available`,
    downloading: `Downloading update… ${state.percent ?? 0}%`,
    downloaded: `eyeris ${state.version} is ready to install`,
    installing: "Restarting to install…",
    error: state.message,
  }[state.status];
  return (
    <aside className="update-status" aria-label="App updates">
      <span aria-live="polite">{text}</span>
      {error && <span role="alert">{error}</span>}
      {state.status === "available" ? (
        <button onClick={() => void action("downloadUpdate")}>
          Download update
        </button>
      ) : state.status === "downloaded" ? (
        <button onClick={() => void action("installUpdate")}>
          Restart and install
        </button>
      ) : ["idle", "current", "error"].includes(state.status) ? (
        <button onClick={() => void action("checkForUpdates")}>
          Check for updates
        </button>
      ) : null}
    </aside>
  );
}
