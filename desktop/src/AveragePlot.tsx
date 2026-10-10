import { useEffect, useRef, useState } from "react";

export interface Series {
  label: string;
  color: string;
  mean: (number | null)[];
  se?: (number | null)[];
  n: number;
}

// A value range that ignores the most extreme 1% at each end, so a single
// artifact does not flatten every other trace.
function robustRange(values: number[]): [number, number] {
  if (!values.length) return [0, 1];
  const sorted = [...values].sort((a, b) => a - b);
  const at = (q: number) =>
    sorted[Math.min(sorted.length - 1, Math.floor(q * sorted.length))];
  return [at(0.01), at(0.99)];
}

// Mean traces with a standard-error band, over each epoch's trace in faded
// gray (a butterfly plot). Missing samples break lines instead of joining them.
export function AveragePlot({
  time,
  traces = [],
  series,
  onset,
  showTraces,
}: {
  time: number[];
  traces?: (number | null)[][];
  series: Series[];
  onset: boolean;
  showTraces: boolean;
}) {
  const canvas = useRef<HTMLCanvasElement>(null);
  const container = useRef<HTMLDivElement>(null);
  const [width, setWidth] = useState(700);
  const [height, setHeight] = useState(320);
  const [hover, setHover] = useState<number | null>(null);
  const left = 66,
    right = 20,
    top = 18,
    bottom = 48;
  const domain: [number, number] = [time[0] ?? 0, time.at(-1) ?? 1];
  const span = domain[1] - domain[0] || 1;
  useEffect(() => {
    const observer = new ResizeObserver((entries) => {
      setWidth(entries[0].contentRect.width);
      setHeight(Math.max(160, entries[0].contentRect.height - 28));
    });
    observer.observe(container.current!);
    return () => observer.disconnect();
  }, []);
  const values: number[] = [];
  for (const s of series)
    s.mean.forEach((m, i) => {
      if (m === null) return;
      const e = s.se?.[i] ?? 0;
      values.push(m - e, m + e);
    });
  const traceValues = showTraces
    ? traces.flat().filter((v): v is number => v !== null)
    : [];
  let [lo, hi] = robustRange([...values, ...traceValues]);
  for (const v of values) {
    lo = Math.min(lo, v);
    hi = Math.max(hi, v);
  }
  const pad = (hi - lo || Math.abs(hi) * 0.1 || 1) * 0.08;
  lo -= pad;
  hi += pad;
  useEffect(() => {
    const el = canvas.current!;
    const ratio = window.devicePixelRatio || 1;
    el.width = width * ratio;
    el.height = height * ratio;
    const ctx = el.getContext("2d")!;
    ctx.scale(ratio, ratio);
    ctx.clearRect(0, 0, width, height);
    const px = (t: number) =>
      left + ((t - domain[0]) / span) * (width - left - right);
    const py = (y: number) =>
      top + (1 - (y - lo) / (hi - lo)) * (height - top - bottom);
    ctx.font = "11px -apple-system, BlinkMacSystemFont, sans-serif";
    for (let i = 0; i <= 4; i++) {
      const y = lo + ((hi - lo) * i) / 4,
        p = py(y);
      ctx.strokeStyle = "#ededed";
      ctx.beginPath();
      ctx.moveTo(left, p);
      ctx.lineTo(width - right, p);
      ctx.stroke();
      ctx.fillStyle = "#737373";
      ctx.textAlign = "right";
      ctx.fillText(
        y.toLocaleString(undefined, { maximumFractionDigits: 2 }),
        left - 12,
        p + 4,
      );
    }
    for (let i = 0; i <= 6; i++) {
      const t = domain[0] + (span * i) / 6;
      ctx.textAlign = "center";
      ctx.fillStyle = "#737373";
      ctx.fillText(t.toFixed(2), px(t), height - 26);
    }
    ctx.fillText(
      onset ? "Time from event onset (s)" : "Time from epoch start (s)",
      (width + left - right) / 2,
      height - 5,
    );
    ctx.save();
    ctx.beginPath();
    ctx.rect(left, top, width - left - right, height - top - bottom);
    ctx.clip();
    if (onset && domain[0] < 0 && domain[1] > 0) {
      ctx.strokeStyle = "#b8b8b8";
      ctx.setLineDash([4, 4]);
      ctx.beginPath();
      ctx.moveTo(px(0), top);
      ctx.lineTo(px(0), height - bottom);
      ctx.stroke();
      ctx.setLineDash([]);
    }
    const line = (ys: (number | null)[]) => {
      ctx.beginPath();
      let drawing = false;
      ys.forEach((y, i) => {
        if (y === null) {
          drawing = false;
          return;
        }
        if (drawing) ctx.lineTo(px(time[i]), py(y));
        else ctx.moveTo(px(time[i]), py(y));
        drawing = true;
      });
      ctx.stroke();
    };
    if (showTraces) {
      ctx.strokeStyle = "rgba(96, 96, 96, 0.16)";
      ctx.lineWidth = 1;
      for (const trace of traces) line(trace);
    }
    for (const s of series) {
      // Fill the band in runs of consecutive finite values.
      if (s.se) {
        ctx.fillStyle = `${s.color}2e`;
        let run: number[] = [];
        const flush = () => {
          if (run.length > 1) {
            ctx.beginPath();
            run.forEach((i, k) => {
              const y = py(s.mean[i]! + (s.se![i] ?? 0));
              if (k) ctx.lineTo(px(time[i]), y);
              else ctx.moveTo(px(time[i]), y);
            });
            for (const i of [...run].reverse())
              ctx.lineTo(px(time[i]), py(s.mean[i]! - (s.se![i] ?? 0)));
            ctx.closePath();
            ctx.fill();
          }
          run = [];
        };
        s.mean.forEach((m, i) => {
          if (m === null) flush();
          else run.push(i);
        });
        flush();
      }
      ctx.strokeStyle = s.color;
      ctx.lineWidth = 2;
      ctx.lineJoin = "round";
      line(s.mean);
    }
    if (hover !== null && hover >= left && hover <= width - right) {
      ctx.strokeStyle = "#999999";
      ctx.setLineDash([3, 4]);
      ctx.beginPath();
      ctx.moveTo(hover, top);
      ctx.lineTo(hover, height - bottom);
      ctx.stroke();
    }
    ctx.restore();
  }, [time, traces, series, width, height, hover, showTraces, onset]);
  let readout = "Hover to read the mean ± standard error";
  if (hover !== null && time.length) {
    const t =
      domain[0] +
      Math.max(0, Math.min(1, (hover - left) / (width - left - right))) * span;
    let i = 0;
    for (let k = 1; k < time.length; k++)
      if (Math.abs(time[k] - t) < Math.abs(time[i] - t)) i = k;
    const format = (v: number | null | undefined) =>
      v === null || v === undefined
        ? "—"
        : v.toLocaleString(undefined, { maximumFractionDigits: 3 });
    readout = `${time[i].toFixed(3)} s · ${series
      .map(
        (s) =>
          `${series.length > 1 ? `${s.label}: ` : ""}${format(s.mean[i])} ± ${format(s.se?.[i])}`,
      )
      .join(" · ")}`;
  }
  return (
    <div className="average-plot" ref={container}>
      <div className="plot-readout">{readout}</div>
      <canvas
        ref={canvas}
        style={{ width: "100%", height }}
        role="img"
        aria-label={`Average pupil trace of ${series.map((s) => `${s.label} (${s.n} epochs)`).join(", ")}`}
        onPointerMove={(e) => setHover(e.nativeEvent.offsetX)}
        onPointerLeave={() => setHover(null)}
      />
    </div>
  );
}
