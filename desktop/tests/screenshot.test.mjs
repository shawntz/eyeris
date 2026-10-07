import assert from "node:assert/strict";
import test from "node:test";
import { captureScreenshot } from "./screenshot.mjs";

const captureError = new Error(
  "page.screenshot: Protocol error (Page.captureScreenshot): Unable to capture screenshot",
);

test("screenshot retries transient compositor failure and keeps capture options", async () => {
  let attempts = 0;
  let activations = 0;
  const waits = [];
  const image = Buffer.from("screenshot");
  const page = {
    async screenshot(options) {
      assert.deepEqual(options, { timeout: 5000, path: "splash.png" });
      if (++attempts < 3) throw captureError;
      return image;
    },
    async bringToFront() {
      activations++;
    },
  };
  assert.equal(
    await captureScreenshot(page, { path: "splash.png" }, async (ms) =>
      waits.push(ms),
    ),
    image,
  );
  assert.equal(activations, 2);
  assert.deepEqual(waits, [100, 250]);
});

test("persistent capture failure remains a test failure after bounded retries", async () => {
  let attempts = 0;
  const page = {
    async screenshot() {
      attempts++;
      throw captureError;
    },
    async bringToFront() {},
  };
  await assert.rejects(
    captureScreenshot(page, {}, async () => {}),
    (error) => error === captureError,
  );
  assert.equal(attempts, 6);
});

test("unrelated screenshot errors fail immediately", async () => {
  const error = new Error("page.screenshot: Target page has been closed");
  const page = {
    async screenshot() {
      throw error;
    },
    async bringToFront() {
      assert.fail("must not retry");
    },
  };
  await assert.rejects(
    captureScreenshot(page, {}),
    (actual) => actual === error,
  );
});
