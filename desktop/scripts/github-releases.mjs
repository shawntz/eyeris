// The tag endpoint only returns published releases. Authenticated release
// listings also include drafts, including uploads from an interrupted publish.
export function findRelease(api, tag, { required = false } = {}) {
  try {
    return api(`releases/tags/${encodeURIComponent(tag)}`);
  } catch (error) {
    if (!String(error.stderr).includes("HTTP 404")) throw error;
  }
  for (let page = 1; ; page++) {
    const releases = api(`releases?per_page=100&page=${page}`);
    const found = releases.find((release) => release.tag_name === tag);
    if (found) return found;
    if (releases.length < 100) break;
  }
  if (required)
    throw new Error(`Release ${tag} was not found after uploading its assets`);
  return null;
}
