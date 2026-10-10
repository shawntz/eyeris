import { execFileSync } from "node:child_process";
import { readFile, writeFile, appendFile } from "node:fs/promises";
import { pathToFileURL } from "node:url";
import semver from "semver";

export const releaseBranch = "dev";
export function releasePlan(version, tags, changedFiles) {
  if (!semver.valid(version) || semver.prerelease(version))
    throw new Error("Automatic releases require a stable desktop version");
  const versions = tags
    .filter((tag) => /^desktop-v\d+\.\d+\.\d+$/.test(tag))
    .map((tag) => tag.slice(9))
    .filter((value) => semver.valid(value))
    .sort(semver.rcompare);
  if (versions[0] && semver.lt(version, versions[0]))
    throw new Error("Desktop manifest is older than the latest desktop tag");
  const relevant = changedFiles.some(
    (file) =>
      file.startsWith("desktop/") ||
      /^\.github\/workflows\/desktop.*\.yml$/.test(file),
  );
  // A manifest newer than the latest desktop tag was set in a PR, for example
  // a minor version. Release that version as it is instead of a patch.
  const next =
    versions[0] && semver.gt(version, versions[0])
      ? version
      : semver.inc(version, "patch");
  return { release: relevant, version: next, tag: `desktop-v${next}` };
}
export function bumpManifests(pkg, lock, version) {
  if (
    pkg.version !== lock.version ||
    pkg.version !== lock.packages?.[""]?.version
  )
    throw new Error("Desktop package and lockfile versions must agree");
  if (semver.inc(pkg.version, "patch") !== version)
    throw new Error(
      "Automatic releases must increment exactly one patch version",
    );
  return [
    { ...pkg, version },
    {
      ...lock,
      version,
      packages: { ...lock.packages, "": { ...lock.packages[""], version } },
    },
  ];
}
export function verifyReleaseCommit(parent, expectedSource, changedFiles) {
  if (parent !== expectedSource)
    throw new Error(
      "dev changed during the version PR merge; do not tag unvalidated code. The next dev run will validate the new head.",
    );
  if (
    changedFiles.length !== 2 ||
    !["desktop/package.json", "desktop/package-lock.json"].every((name) =>
      changedFiles.includes(name),
    )
  )
    throw new Error(
      "The version commit must change only the desktop manifests",
    );
}
const git = (...args) => execFileSync("git", args, { encoding: "utf8" }).trim();
const lines = (value) => (value ? value.split("\n") : []);
async function output(values) {
  await appendFile(
    process.env.GITHUB_OUTPUT,
    Object.entries(values)
      .map(([key, value]) => `${key}=${value}\n`)
      .join(""),
  );
}
async function plan() {
  const source = git("rev-parse", "HEAD");
  const tags = lines(git("tag", "--merged", "HEAD", "--list", "desktop-v*"));
  const latest = tags
    .filter(
      (tag) =>
        /^desktop-v\d+\.\d+\.\d+$/.test(tag) && semver.valid(tag.slice(9)),
    )
    .sort((a, b) => semver.rcompare(a.slice(9), b.slice(9)))[0];
  // Compare against the last desktop tag, so coalesced pushes cannot lose a
  // desktop change when the newest push only touches R or documentation.
  const changed = latest
    ? lines(git("diff", "--name-only", latest, source))
    : lines(git("ls-tree", "-r", "--name-only", source));
  const pkg = JSON.parse(await readFile("desktop/package.json", "utf8"));
  await output({ ...releasePlan(pkg.version, tags, changed), source });
}
export async function prepare({ api: suppliedApi } = {}) {
  const source = process.env.DESKTOP_SOURCE;
  const tag = process.env.DESKTOP_TAG;
  const repository = process.env.GITHUB_REPOSITORY;
  if (
    !/^[0-9a-f]{40}$/.test(source || "") ||
    !/^desktop-v\d+\.\d+\.\d+$/.test(tag || "")
  )
    throw new Error("Invalid release source or tag");
  const version = tag.slice(9);
  const branch = `automation/${tag}`;
  const api =
    suppliedApi ||
    ((route, method = "GET", data) =>
      JSON.parse(
        execFileSync(
          "gh",
          [
            "api",
            `repos/${repository}/${route}`,
            "--method",
            method,
            ...(data ? ["--input", "-"] : []),
          ],
          { encoding: "utf8", input: data ? JSON.stringify(data) : undefined },
        ),
      ));
  git("fetch", "origin", releaseBranch, "--tags");
  // A planned version is already in the validated commit's manifests: tag
  // that commit itself, with no version PR.
  const planned = JSON.parse(git("show", `${source}:desktop/package.json`));
  if (planned.version === version) {
    const lock = JSON.parse(git("show", `${source}:desktop/package-lock.json`));
    if (lock.version !== version || lock.packages?.[""]?.version !== version)
      throw new Error("Desktop package and lockfile versions must agree");
    if (git("rev-parse", `origin/${releaseBranch}`) !== source) {
      console.log("A newer dev push is queued; skip this obsolete candidate.");
      await output({ release: false });
      return;
    }
    if (lines(git("tag", "--list", tag)).length) {
      if (git("rev-parse", `${tag}^{commit}`) !== source)
        throw new Error("Desktop tag already points at a different commit");
    } else {
      git("tag", tag, source);
      git("push", "origin", `refs/tags/${tag}`);
    }
    await output({ release: true, tag });
    return;
  }
  const prs = api(
    `pulls?state=all&base=${releaseBranch}&head=${repository.split("/")[0]}:${branch}`,
  );
  let pr = prs[0];
  let merged;
  if (pr) pr = api(`pulls/${pr.number}`);
  if (pr?.merged) {
    // Rerun only failed jobs to resume an interrupted tag/publish step without
    // making another version commit.
    merged = pr.merge_commit_sha;
  } else {
    if (git("rev-parse", `origin/${releaseBranch}`) !== source) {
      console.log("A newer dev push is queued; skip this obsolete candidate.");
      await output({ release: false });
      return;
    }
    if (pr?.state === "closed")
      throw new Error("The automatic version PR was closed without merging");
    git("checkout", "--detach", source);
    const pkg = JSON.parse(await readFile("desktop/package.json", "utf8"));
    const lock = JSON.parse(
      await readFile("desktop/package-lock.json", "utf8"),
    );
    const manifests = bumpManifests(pkg, lock, version);
    const remoteBranch = git("ls-remote", "--heads", "origin", branch);
    let head = remoteBranch.split(/\s+/)[0];
    let reuse = false;
    if (head) {
      git("fetch", "origin", branch);
      const parent = git("rev-parse", `${head}^`);
      // Never replace a branch with changes outside the two version manifests.
      verifyReleaseCommit(
        parent,
        parent,
        lines(git("diff", "--name-only", parent, head)),
      );
      const previous = bumpManifests(
        JSON.parse(git("show", `${parent}:desktop/package.json`)),
        JSON.parse(git("show", `${parent}:desktop/package-lock.json`)),
        version,
      );
      for (const [index, name] of [
        "desktop/package.json",
        "desktop/package-lock.json",
      ].entries())
        if (
          JSON.stringify(JSON.parse(git("show", `${head}:${name}`))) !==
          JSON.stringify(previous[index])
        )
          throw new Error(
            "Existing version branch has unexpected manifest edits",
          );
      reuse = parent === source;
    }
    if (!reuse) {
      for (const [index, name] of [
        "desktop/package.json",
        "desktop/package-lock.json",
      ].entries())
        await writeFile(name, JSON.stringify(manifests[index], null, 2) + "\n");
      git("config", "user.name", "github-actions[bot]");
      git(
        "config",
        "user.email",
        "41898282+github-actions[bot]@users.noreply.github.com",
      );
      git("add", "desktop/package.json", "desktop/package-lock.json");
      git("commit", "-m", `chore(desktop): release ${version}`);
      // Resume an orphaned branch or refresh an obsolete bot PR only while its
      // expected head still matches; never force-push the protected dev branch.
      git(
        "push",
        "origin",
        `--force-with-lease=refs/heads/${branch}:${head || ""}`,
        `HEAD:refs/heads/${branch}`,
      );
      head = git("rev-parse", "HEAD");
    }
    if (!pr)
      pr = api("pulls", "POST", {
        base: releaseBranch,
        head: branch,
        title: `chore(desktop): release ${version}`,
        body: `Increment the desktop patch version after native CI passed. Only desktop/package.json and its lockfile change. The R package version is unchanged.\n\nWorkflow: https://github.com/${repository}/actions/runs/${process.env.GITHUB_RUN_ID}`,
      });
    for (let attempt = 0; attempt < 30; attempt++) {
      pr = api(`pulls/${pr.number}`);
      if (pr.mergeable !== null) break;
      await new Promise((resolve) => setTimeout(resolve, 1000));
    }
    git("fetch", "origin", releaseBranch);
    if (git("rev-parse", `origin/${releaseBranch}`) !== source) {
      console.log(
        "dev advanced; leave the version PR for the next validated run.",
      );
      await output({ release: false });
      return;
    }
    const result = api(`pulls/${pr.number}/merge`, "PUT", {
      merge_method: "squash",
      sha: head,
      commit_title: `chore(desktop): release ${version}`,
      commit_message: `Desktop source: ${source}`,
    });
    if (!result.merged)
      throw new Error(result.message || "Version PR could not be merged");
    merged = result.sha;
  }
  git("fetch", "origin", releaseBranch, "--tags");
  verifyReleaseCommit(
    git("rev-parse", `${merged}^`),
    source,
    lines(git("diff", "--name-only", `${merged}^`, merged)),
  );
  const publishedPkg = JSON.parse(
    git("show", `${merged}:desktop/package.json`),
  );
  const publishedLock = JSON.parse(
    git("show", `${merged}:desktop/package-lock.json`),
  );
  const expected = bumpManifests(
    JSON.parse(git("show", `${source}:desktop/package.json`)),
    JSON.parse(git("show", `${source}:desktop/package-lock.json`)),
    version,
  );
  if (
    JSON.stringify(publishedPkg) !== JSON.stringify(expected[0]) ||
    JSON.stringify(publishedLock) !== JSON.stringify(expected[1])
  )
    throw new Error(
      "Merged manifests do not match the expected version-only change",
    );
  const existing = lines(git("tag", "--list", tag));
  if (existing.length) {
    if (git("rev-parse", `${tag}^{commit}`) !== merged)
      throw new Error("Desktop tag already points at a different commit");
  } else {
    git("tag", tag, merged);
    git("push", "origin", `refs/tags/${tag}`);
  }
  await output({ release: true, tag });
}
if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  if (process.argv[2] === "plan") await plan();
  else if (process.argv[2] === "prepare") await prepare();
  else throw new Error("Use auto-release.mjs plan or prepare");
}
