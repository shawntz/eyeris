import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtemp, mkdir, writeFile, readFile, rm } from "node:fs/promises";
import { execFileSync } from "node:child_process";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import yaml from "js-yaml";
import {
  releasePlan,
  bumpManifests,
  verifyReleaseCommit,
} from "../scripts/auto-release.mjs";

test("desktop changes get patch tags independently of CRAN; R-only changes do not", () => {
  const tags = [
    "v3.3.0",
    "desktop-latest",
    "desktop-v0.3.0",
    "desktop-v0.4.0-beta.1",
  ];
  assert.deepEqual(releasePlan("0.3.0", tags, ["desktop/electron/main.mjs"]), {
    release: true,
    version: "0.3.1",
    tag: "desktop-v0.3.1",
  });
  assert.equal(
    releasePlan("0.3.0", tags, ["R/foo.R", "DESCRIPTION"]).release,
    false,
  );
  assert.equal(
    releasePlan("0.3.0", tags, [".github/workflows/desktop.yml"]).release,
    true,
  );
  assert.throws(() => releasePlan("0.2.0", tags, ["desktop/foo"]), /older/);
  // A version set ahead of the latest tag, such as a minor release, is
  // released as it is; once tagged, later changes are patches again.
  assert.deepEqual(releasePlan("0.4.0", tags, ["desktop/package.json"]), {
    release: true,
    version: "0.4.0",
    tag: "desktop-v0.4.0",
  });
  assert.equal(
    releasePlan("0.4.0", [...tags, "desktop-v0.4.0"], ["desktop/foo"]).version,
    "0.4.1",
  );
  assert.throws(
    () => releasePlan("0.3.1-beta.1", tags, ["desktop/foo"]),
    /stable/,
  );
});
test("version changes preserve dependencies and reject inconsistent or non-patch bumps", () => {
  const pkg = {
    name: "eyeris-desktop",
    version: "0.3.0",
    dependencies: { react: "19" },
  };
  const lock = {
    version: "0.3.0",
    packages: { "": { ...pkg }, "node_modules/react": { version: "19" } },
  };
  const [next, nextLock] = bumpManifests(pkg, lock, "0.3.1");
  assert.equal(next.version, "0.3.1");
  assert.equal(nextLock.version, "0.3.1");
  assert.equal(nextLock.packages[""].version, "0.3.1");
  assert.deepEqual(next.dependencies, pkg.dependencies);
  assert.deepEqual(
    nextLock.packages["node_modules/react"],
    lock.packages["node_modules/react"],
  );
  assert.equal(pkg.version, "0.3.0");
  assert.throws(
    () => bumpManifests(pkg, { ...lock, version: "0.2.0" }, "0.3.1"),
    /agree/,
  );
  assert.throws(() => bumpManifests(pkg, lock, "0.4.0"), /patch/);
});
test("release tags cannot include unvalidated concurrent edits", () => {
  const files = ["desktop/package.json", "desktop/package-lock.json"];
  verifyReleaseCommit("validated", "validated", files);
  assert.throws(
    () => verifyReleaseCommit("newer", "validated", files),
    /changed during/,
  );
  assert.throws(
    () => verifyReleaseCommit("validated", "validated", [...files, "R/foo.R"]),
    /only/,
  );
});
test("planning compares against the desktop tag and catches coalesced pushes", async () => {
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-auto-release-"));
  const git = (...args) =>
    execFileSync("git", args, { cwd: dir, encoding: "utf8" }).trim();
  try {
    git("init", "-b", "dev");
    git("config", "user.email", "test@example.com");
    git("config", "user.name", "Release test");
    await mkdir(path.join(dir, "desktop"));
    await writeFile(
      path.join(dir, "desktop/package.json"),
      JSON.stringify({ version: "0.3.0" }),
    );
    git("add", ".");
    git("commit", "-m", "Initial");
    git("tag", "desktop-v0.3.0");
    const script = fileURLToPath(
      new URL("../scripts/auto-release.mjs", import.meta.url),
    );
    const output = path.join(dir, "outputs");
    const plan = async () => {
      await writeFile(output, "");
      execFileSync(process.execPath, [script, "plan"], {
        cwd: dir,
        env: { ...process.env, GITHUB_OUTPUT: output },
      });
      return readFile(output, "utf8");
    };
    assert.match(await plan(), /release=false/);
    await writeFile(path.join(dir, "desktop/changed.txt"), "changed");
    git("add", "desktop");
    git("commit", "-m", "Desktop change");
    await writeFile(path.join(dir, "README.md"), "R docs");
    git("add", "README.md");
    git("commit", "-m", "Later unrelated push");
    assert.match(
      await plan(),
      /release=true\nversion=0.3.1\ntag=desktop-v0.3.1/,
    );
    await writeFile(
      path.join(dir, "desktop/package.json"),
      JSON.stringify({ version: "0.4.0" }),
    );
    git("commit", "-am", "Plan the 0.4.0 release");
    assert.match(
      await plan(),
      /release=true\nversion=0.4.0\ntag=desktop-v0.4.0/,
    );
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
});
test("automatic releases gate version writes on CI and call publishing directly with isolated artifacts", async () => {
  const workflow = async (name) =>
    yaml.load(
      await readFile(
        new URL(`../../.github/workflows/${name}`, import.meta.url),
        "utf8",
      ),
    );
  const auto = await workflow("desktop-auto-release.yml");
  const release = await workflow("desktop-release.yml");
  const build = await workflow("desktop.yml");
  assert.deepEqual(auto.on.push.branches, ["dev"]);
  assert.equal(auto.on.pull_request, undefined);
  assert.deepEqual(auto.jobs.version.needs, ["plan", "validate"]);
  assert.equal(auto.jobs.version.permissions["pull-requests"], "write");
  assert.equal(
    auto.jobs.release.uses,
    "./.github/workflows/desktop-release.yml",
  );
  assert.equal(auto.jobs.release.with.ref, "${{ needs.version.outputs.tag }}");
  assert.equal(release.jobs.build.with["artifact-prefix"], "desktop-signed");
  assert.equal(
    build.on.workflow_call.inputs["artifact-prefix"].default,
    "desktop",
  );
  assert.equal(
    release.jobs.publish.steps.find(
      (step) => step.uses === "actions/download-artifact@v4",
    ).with.pattern,
    "desktop-signed-release-*",
  );
});

test("version PR preparation commits, merges, tags, and resumes without another bump", async () => {
  const { prepare } = await import("../scripts/auto-release.mjs");
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-version-pr-"));
  const previousCwd = process.cwd();
  const variables = [
    "DESKTOP_SOURCE",
    "DESKTOP_TAG",
    "GITHUB_REPOSITORY",
    "GITHUB_RUN_ID",
    "GITHUB_OUTPUT",
  ];
  const previousEnv = Object.fromEntries(
    variables.map((name) => [name, process.env[name]]),
  );
  const remote = path.join(dir, "remote.git");
  const checkout = path.join(dir, "checkout");
  const merger = path.join(dir, "merger");
  const gitAt = (cwd, ...args) =>
    execFileSync("git", args, {
      cwd,
      encoding: "utf8",
      stdio: ["pipe", "pipe", "pipe"],
    }).trim();
  const git = (...args) => gitAt(checkout, ...args);
  try {
    gitAt(dir, "init", "--bare", remote);
    gitAt(dir, "clone", remote, checkout);
    git("checkout", "-b", "dev");
    git("config", "user.name", "Test");
    git("config", "user.email", "test@example.com");
    await mkdir(path.join(checkout, "desktop"));
    const pkg = { name: "eyeris-desktop", version: "0.3.0" };
    await writeFile(
      path.join(checkout, "desktop/package.json"),
      JSON.stringify(pkg, null, 2) + "\n",
    );
    await writeFile(
      path.join(checkout, "desktop/package-lock.json"),
      JSON.stringify({ version: pkg.version, packages: { "": pkg } }, null, 2) +
        "\n",
    );
    git("add", ".");
    git("commit", "-m", "Validated desktop change");
    git("push", "origin", "dev");
    const source = git("rev-parse", "HEAD");
    Object.assign(process.env, {
      DESKTOP_SOURCE: source,
      DESKTOP_TAG: "desktop-v0.3.1",
      GITHUB_REPOSITORY: "owner/repo",
      GITHUB_RUN_ID: "1",
      GITHUB_OUTPUT: path.join(dir, "outputs"),
    });
    let pr;
    const github = (route, method = "GET", data) => {
      if (route.startsWith("pulls?")) return pr ? [pr] : [];
      if (route === "pulls" && method === "POST") {
        assert.equal(data.base, "dev");
        assert.equal(data.head, "automation/desktop-v0.3.1");
        pr = {
          number: 1,
          state: "open",
          merged: false,
          mergeable: true,
          head: { sha: git("rev-parse", "HEAD") },
        };
        return pr;
      }
      if (route === "pulls/1" && method === "GET") return pr;
      if (route === "pulls/1/merge" && method === "PUT") {
        assert.equal(data.sha, pr.head.sha);
        gitAt(dir, "clone", "--branch", "dev", remote, merger);
        gitAt(merger, "config", "user.name", "Test merger");
        gitAt(merger, "config", "user.email", "test@example.com");
        gitAt(merger, "merge", "--squash", "origin/automation/desktop-v0.3.1");
        gitAt(merger, "commit", "-m", data.commit_title);
        gitAt(merger, "push", "origin", "dev");
        pr = {
          ...pr,
          merged: true,
          state: "closed",
          merge_commit_sha: gitAt(merger, "rev-parse", "HEAD"),
        };
        return { merged: true, sha: pr.merge_commit_sha };
      }
      throw new Error(`Unexpected API call: ${method} ${route}`);
    };
    process.chdir(checkout);
    await prepare({ api: github });
    const tag = git("rev-parse", "desktop-v0.3.1");
    assert.equal(tag, git("rev-parse", "origin/dev"));
    assert.equal(git("rev-parse", `${tag}^`), source);
    assert.equal(
      JSON.parse(git("show", `${tag}:desktop/package.json`)).version,
      "0.3.1",
    );
    assert.match(
      await readFile(process.env.GITHUB_OUTPUT, "utf8"),
      /release=true\ntag=desktop-v0.3.1/,
    );
    await prepare({ api: github });
    assert.equal(git("rev-parse", "origin/dev"), tag);
    assert.equal(git("rev-list", "--count", "origin/dev"), "2");
  } finally {
    process.chdir(previousCwd);
    for (const name of variables) {
      if (previousEnv[name] === undefined) delete process.env[name];
      else process.env[name] = previousEnv[name];
    }
    await rm(dir, { recursive: true, force: true });
  }
});

test("a planned version is tagged on its validated commit without a version PR", async () => {
  const { prepare } = await import("../scripts/auto-release.mjs");
  const dir = await mkdtemp(path.join(tmpdir(), "eyeris-planned-release-"));
  const previousCwd = process.cwd();
  const variables = [
    "DESKTOP_SOURCE",
    "DESKTOP_TAG",
    "GITHUB_REPOSITORY",
    "GITHUB_OUTPUT",
  ];
  const previousEnv = Object.fromEntries(
    variables.map((name) => [name, process.env[name]]),
  );
  const remote = path.join(dir, "remote.git");
  const checkout = path.join(dir, "checkout");
  const gitAt = (cwd, ...args) =>
    execFileSync("git", args, {
      cwd,
      encoding: "utf8",
      stdio: ["pipe", "pipe", "pipe"],
    }).trim();
  const git = (...args) => gitAt(checkout, ...args);
  const commit = async (version, lockVersion = version) => {
    const pkg = { name: "eyeris-desktop", version };
    await writeFile(
      path.join(checkout, "desktop/package.json"),
      JSON.stringify(pkg, null, 2) + "\n",
    );
    await writeFile(
      path.join(checkout, "desktop/package-lock.json"),
      JSON.stringify(
        {
          version: lockVersion,
          packages: { "": { ...pkg, version: lockVersion } },
        },
        null,
        2,
      ) + "\n",
    );
    git("add", ".");
    git("commit", "-m", `Desktop ${version}`);
    git("push", "origin", "dev");
    return git("rev-parse", "HEAD");
  };
  const run = async (source, tag) => {
    Object.assign(process.env, {
      DESKTOP_SOURCE: source,
      DESKTOP_TAG: tag,
      GITHUB_REPOSITORY: "owner/repo",
      GITHUB_OUTPUT: path.join(dir, "outputs"),
    });
    await writeFile(process.env.GITHUB_OUTPUT, "");
    // No version PR is created or looked up.
    await prepare({
      api: (route, method = "GET") => {
        throw new Error(`Unexpected API call: ${method} ${route}`);
      },
    });
    return readFile(process.env.GITHUB_OUTPUT, "utf8");
  };
  try {
    gitAt(dir, "init", "--bare", remote);
    gitAt(dir, "clone", remote, checkout);
    git("checkout", "-b", "dev");
    git("config", "user.name", "Test");
    git("config", "user.email", "test@example.com");
    await mkdir(path.join(checkout, "desktop"));
    const source = await commit("0.4.0");
    process.chdir(checkout);
    assert.match(
      await run(source, "desktop-v0.4.0"),
      /release=true\ntag=desktop-v0.4.0/,
    );
    assert.equal(git("rev-parse", "desktop-v0.4.0^{commit}"), source);
    assert.match(git("ls-remote", "--tags", "origin"), /desktop-v0.4.0/);
    assert.equal(git("rev-list", "--count", "origin/dev"), "1");
    // Rerunning after a later failure keeps the same tag.
    assert.match(await run(source, "desktop-v0.4.0"), /release=true/);
    const mismatched = await commit("0.5.0", "0.4.0");
    await assert.rejects(() => run(mismatched, "desktop-v0.5.0"), /agree/);
    // A newer dev push is validated by its own run.
    const obsolete = await commit("0.6.0");
    await commit("0.6.0-dev");
    assert.match(await run(obsolete, "desktop-v0.6.0"), /release=false/);
    assert.equal(git("tag", "--list", "desktop-v0.6.0"), "");
  } finally {
    process.chdir(previousCwd);
    for (const name of variables) {
      if (previousEnv[name] === undefined) delete process.env[name];
      else process.env[name] = previousEnv[name];
    }
    await rm(dir, { recursive: true, force: true });
  }
});
