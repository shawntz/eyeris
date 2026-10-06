import { useEffect, useRef, useState } from "react";
import type { Trace } from "./types";

export function TracePlot({
  trace,
  range,
  onRange,
}: {
  trace: Trace;
  range?: [number, number];
  onRange: (range?: [number, number]) => void;
}) {
  const canvas = useRef<HTMLCanvasElement>(null);
  const container = useRef<HTMLDivElement>(null);
  const [width, setWidth] = useState(700);
  const [height, setHeight] = useState(250);
  const [hover, setHover] = useState<number | null>(null);
  const [drag, setDrag] = useState<[number, number] | null>(null);
  const bounds = range || trace.domain;
  const left = 62,
    right = 20,
    top = 26,
    bottom = 48;
  const span = bounds[1] - bounds[0] || 1;
  const timeAt = (x: number) =>
    bounds[0] +
    Math.max(0, Math.min(1, (x - left) / (width - left - right))) * span;
  useEffect(() => {
    const observer = new ResizeObserver((entries) => {
      setWidth(entries[0].contentRect.width);
      setHeight(Math.max(80, entries[0].contentRect.height - 24));
    });
    observer.observe(container.current!);
    return () => observer.disconnect();
  }, []);
  useEffect(() => {
    const el = canvas.current!;
    const ratio = window.devicePixelRatio || 1;
    el.width = width * ratio;
    el.height = height * ratio;
    const ctx = el.getContext("2d")!;
    ctx.scale(ratio, ratio);
    ctx.clearRect(0, 0, width, height);
    const values = trace.signal.filter(
      (y, i): y is number =>
        y !== null && trace.time[i] >= bounds[0] && trace.time[i] <= bounds[1],
    );
    let lo = Infinity,
      hi = -Infinity;
    for (const y of values) {
      lo = Math.min(lo, y);
      hi = Math.max(hi, y);
    }
    if (!values.length) {
      lo = 0;
      hi = 1;
    }
    const pad = (hi - lo || Math.abs(hi) * 0.1 || 1) * 0.14;
    lo -= pad;
    hi += pad;
    const px = (t: number) =>
      left + ((t - bounds[0]) / span) * (width - left - right);
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
    for (let i = 0; i <= 5; i++) {
      const t = bounds[0] + (span * i) / 5;
      ctx.textAlign = "center";
      ctx.fillStyle = "#737373";
      ctx.fillText(t.toFixed(2), px(t), height - 26);
    }
    ctx.fillStyle = "#737373";
    ctx.fillText(
      "Time from epoch start (s)",
      (width + left - right) / 2,
      height - 5,
    );
    ctx.save();
    ctx.beginPath();
    ctx.rect(left, top, width - left - right, height - top - bottom);
    ctx.clip();
    // Missing intervals remain broken lines, including after display reduction.
    ctx.strokeStyle = "#820000";
    ctx.lineWidth = 1.8;
    ctx.lineJoin = "round";
    ctx.beginPath();
    let drawing = false;
    trace.time.forEach((t, i) => {
      const y = trace.signal[i];
      if (y === null) {
        drawing = false;
        return;
      }
      if (drawing) ctx.lineTo(px(t), py(y));
      else ctx.moveTo(px(t), py(y));
      drawing = true;
    });
    ctx.stroke();
    if (drag) {
      ctx.fillStyle = "#82000020";
      ctx.fillRect(
        Math.min(...drag),
        top,
        Math.abs(drag[1] - drag[0]),
        height - top - bottom,
      );
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
    if (!values.length) {
      ctx.fillStyle = "#8d7568";
      ctx.textAlign = "center";
      ctx.fillText(
        "No finite signal samples in this window",
        width / 2,
        height / 2,
      );
    }
  }, [trace, width, height, hover, drag, range]);
  let hoverLabel = "";
  if (hover !== null) {
    const t = timeAt(hover);
    let closest = 0;
    for (let i = 1; i < trace.time.length; i++)
      if (Math.abs(trace.time[i] - t) < Math.abs(trace.time[closest] - t))
        closest = i;
    hoverLabel = `${(trace.time[closest] ?? t).toFixed(3)} s · ${trace.signal[closest]?.toLocaleString(undefined, { maximumFractionDigits: 3 }) ?? "missing"}`;
  }
  return (
    <div className="trace-wrap" ref={container}>
      <div className="plot-readout">
        {hoverLabel ||
          "Hover to inspect · drag to zoom · double-click to reset"}
      </div>
      <canvas
        ref={canvas}
        style={{ width: "100%", height }}
        role="img"
        aria-label={`Pupil signal, ${trace.samples} samples, ${(trace.missing * 100).toFixed(1)} percent missing. Drag to zoom.`}
        onPointerDown={(e) => {
          const x = e.nativeEvent.offsetX;
          e.currentTarget.setPointerCapture(e.pointerId);
          setDrag([x, x]);
        }}
        onPointerMove={(e) => {
          const x = e.nativeEvent.offsetX;
          setHover(x);
          if (drag) setDrag([drag[0], x]);
        }}
        onPointerLeave={() => setHover(null)}
        onPointerUp={() => {
          if (drag && Math.abs(drag[1] - drag[0]) > 12)
            onRange([timeAt(Math.min(...drag)), timeAt(Math.max(...drag))]);
          setDrag(null);
        }}
        onPointerCancel={() => setDrag(null)}
        onDoubleClick={() => onRange(undefined)}
      />
    </div>
  );
}
