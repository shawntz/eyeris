import { useEffect, useState } from "react";
import type { EventPatterns } from "./types";

// Event patterns found in these recordings' messages, refreshed while the
// recordings are still being read in the background.
export function useEventPatterns(ids: string[], enabled: boolean) {
  const [patterns, setPatterns] = useState<EventPatterns | null>(null);
  const key = ids.join(",");
  useEffect(() => {
    if (!enabled || !ids.length) {
      setPatterns(null);
      return;
    }
    let alive = true;
    let timer: ReturnType<typeof setTimeout>;
    const load = () =>
      window.eyeris
        .eventPatterns(ids)
        .then((result) => {
          if (!alive) return;
          setPatterns(result);
          if (result.pending) timer = setTimeout(load, 1000);
        })
        .catch(() => {});
    void load();
    return () => {
      alive = false;
      clearTimeout(timer);
    };
  }, [key, enabled]);
  return patterns;
}
