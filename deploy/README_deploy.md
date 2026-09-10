# ParnellBenchmark — deployment handoff (mysynch platform team)

A single-container Shiny app: three client herds' reproductive KPIs vs. an
anonymized 457-herd peer population.

## Build & run

From the repo root (the `.dockerignore` whitelist there is load-bearing):

```bash
docker build -f deploy/Dockerfile -t parnell-benchmark .
docker run -p 3838:3838 parnell-benchmark
```

App serves on port **3838**. Startup is a few seconds; steady-state RAM well
under 1 GB (all data is preaggregated). No environment variables required.

## What data is in the image

- `data/parnell_files/benchmark_data_v2.rds` (~3 MB) — anonymized peer
  summary: monthly aggregate tables per herd labeled Peer-001…457. Built by
  `build_parnell_benchmark_v2.R`; identifying columns are stripped and
  assertion-checked at build time.
- `data/parnell_files/own_data.rds` (<1 MB) — the client herds' monthly
  aggregates, labeled by herd-key prefix (e.g. "Herd 2a121f57"). Built by
  `build_own_benchmark.R`.
- **No cow-level records anywhere in the image.** The `.dockerignore` is a
  whitelist; raw exports and intermediate parquet cannot enter the build
  context.

## Access control

The app has no built-in authentication. Host it behind the platform's usual
reverse proxy / identity layer; the own-herd labels are meaningful to anyone
who knows Parnell herd keys.

## Refreshing data

1. Own herds: rerun the pipeline (step0) on new exports, then
   `Rscript build_own_benchmark.R`.
2. Peers: download the silver files, then `SILVER_DIR=<dir> Rscript
   build_parnell_benchmark_v2.R` (supports chunked runs via
   HERD_START/HERD_END/CHUNK_DIR/FINALIZE env vars).
3. `docker build` again — data ships in the image by design.

Contact: Nora Schrag (nora.schrag@parnellgroup.com).
