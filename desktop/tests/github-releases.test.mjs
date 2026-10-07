import { test } from "node:test";
import assert from "node:assert/strict";
import { findRelease } from "../scripts/github-releases.mjs";

const missing = () =>
  Object.assign(new Error("Not Found"), { stderr: "gh: Not Found (HTTP 404)" });

test("published release lookup retains asset digests for immutable checks", () => {
  const published = {
    id: 10,
    draft: false,
    assets: [{ digest: "sha256:abc" }],
  };
  assert.equal(
    findRelease((route) => {
      assert.equal(route, "releases/tags/desktop-v0.3.5");
      return published;
    }, "desktop-v0.3.5"),
    published,
  );
});

for (const tag of ["desktop-v0.3.5", "desktop-latest"]) {
  test(`finds the uploaded ${tag} draft when the tag endpoint returns 404`, () => {
    const draft = { id: 20, tag_name: tag, draft: true, assets: [{ id: 30 }] };
    const routes = [];
    const api = (route) => {
      routes.push(route);
      if (route.startsWith("releases/tags/")) throw missing();
      return [{ tag_name: "unrelated" }, draft];
    };
    // Initial lookup permits resuming a draft; post-upload lookup must find it.
    assert.equal(findRelease(api, tag), draft);
    assert.equal(findRelease(api, tag, { required: true }), draft);
    assert.deepEqual(routes, [
      `releases/tags/${tag}`,
      "releases?per_page=100&page=1",
      `releases/tags/${tag}`,
      "releases?per_page=100&page=1",
    ]);
  });
}

test("searches beyond the first page without confusing unrelated releases", () => {
  const draft = { id: 42, tag_name: "desktop-latest", draft: true };
  const routes = [];
  const found = findRelease((route) => {
    routes.push(route);
    if (route.startsWith("releases/tags/")) throw missing();
    return route.endsWith("page=1")
      ? Array.from({ length: 100 }, (_, id) => ({ tag_name: `v${id}` }))
      : [draft];
  }, "desktop-latest");
  assert.equal(found, draft);
  assert.equal(routes.at(-1), "releases?per_page=100&page=2");
});

test("a nonexistent release may be created, but cannot be published by ID", () => {
  const api = (route) => {
    if (route.startsWith("releases/tags/")) throw missing();
    return [];
  };
  assert.equal(findRelease(api, "desktop-latest"), null);
  assert.throws(
    () => findRelease(api, "desktop-latest", { required: true }),
    /Release desktop-latest was not found after uploading its assets/,
  );
});

test("authentication and server errors do not masquerade as missing releases", () => {
  for (const status of [401, 403, 500]) {
    const error = Object.assign(new Error("API failed"), {
      stderr: `gh: API failed (HTTP ${status})`,
    });
    let calls = 0;
    assert.throws(
      () =>
        findRelease(() => {
          calls++;
          throw error;
        }, "desktop-latest"),
      (caught) => caught === error,
    );
    assert.equal(calls, 1);
  }
});

test("draft listing failures propagate instead of attempting duplicate creation", () => {
  const error = new Error("Listing failed");
  assert.throws(
    () =>
      findRelease((route) => {
        if (route.startsWith("releases/tags/")) throw missing();
        throw error;
      }, "desktop-latest"),
    (caught) => caught === error,
  );
});
