import { setTimeout as delay } from "node:timers/promises";

// Electron's compositor can briefly reject capture on macOS CI even after the
// DOM is visible. Retry only that protocol error, not assertions or timeouts.
export async function captureScreenshot(page, options, pause = delay) {
  const intervals = [100, 250, 500, 1000, 2000];
  for (let attempt = 0; ; attempt++) {
    try {
      return await page.screenshot({ timeout: 5000, ...options });
    } catch (error) {
      if (
        !error.message?.includes(
          "Protocol error (Page.captureScreenshot): Unable to capture screenshot",
        ) ||
        attempt === intervals.length
      ) {
        throw error;
      }
      await page.bringToFront();
      await pause(intervals[attempt]);
    }
  }
}
