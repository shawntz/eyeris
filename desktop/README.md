# eyeris Desktop review engine

Local R worker and SQLite project storage for reviewing epoched eyeris RDS files.
The Electron interface and installers are introduced by later PRs in this series.

Requires Node.js 22.13+ and R with jsonlite, eyeris dependencies, and pkgload.

```sh
cd desktop
npm ci
npm test
```

Projects retain immutable, hashed source files, epoch identities, review decisions,
audit history, and lossless RDS/CSV exports. Tests exercise repeated trial IDs,
10,001-epoch paging, trace reduction, persistence, undo, source tampering, and
export fidelity. R discovery supports Windows, macOS, and Linux and can be
overridden with EYERIS_RSCRIPT.

## Processing engine

Subject/session/task recordings feed isolated R processing jobs. Each run stores its
configuration, logs, version provenance, replay script, RDS, and BIDS outputs.
Cancellation stops publication; reruns retain complete output without overwriting
existing shared BIDS files. Tests cover real ASC input, reports, DuckDB, epochs,
reprocessing, failures, and cancellation. HTML reports require Pandoc.
