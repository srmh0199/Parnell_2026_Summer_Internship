# Worklog

Newest entry on top. Dates are absolute.

## 2026-07-22 — Wrote repo context down so it survives outside chat

**Done:** Added a root `CLAUDE.md` describing the pipeline (step0 → step1 → step2 →
step3 → `parnell_repro.qmd`), the KPI definitions the report actually encodes, and a
status/open-items list. Started this worklog. No analysis code was changed.

**Why:** All of the reasoning behind this project — what counts as an eligible cow,
why abortions are counted as conceptions, why the last two periods are dropped from
the graphs — lived only in chat sessions and in the heads of the two people working
on it. Anyone cloning the remote repo (or any future Claude session opened on it)
started from zero. The context now travels with the repo.

**Note on provenance:** The pipeline description and KPI definitions were read
directly out of the code, so they're accurate to what runs today. The *intent*
behind a few choices (e.g. `vwp <- 50`, the 50/75/90% benchmark lines) was inferred
from context, not confirmed by Sarah or Nora — correct them here if they're wrong.

**Gotchas found while writing this up (all pre-existing, none fixed):**
- `ParnellRepro/app.R` references `df_herd` / `lactation_group` / `pregnancy_status` /
  `days_in_milk`, none of which exist. The app cannot run as written.
- `monthly_report.qmd` calls `date_min_pull()` as a function (it's a Date) and points
  at a logo path that doesn't exist in this repo. It cannot render.
- `parnell_repro.qmd` asks for `images/parnell.jpeg`; the repo has `images/parnell.png`.
- `fxn_load_os_fxns()` sources functions from GitHub at render time — no internet, no render.
- The raw ~62 MB `Monte Vista Dairy.csv` is committed. Convenient for reproducibility,
  but it's real client data in a hosted repo; worth a deliberate decision.

**Next:** Validate the pregnancy-rate numbers against DC305's own repro summary for
Monte Vista — the PR graph has looked wrong since 2026-07-21 (commit `c364545`) and
nothing has been checked against an external source yet.

## Next session

```text
Resume work on the Parnell 2026 Summer Internship repo (C:\GIT\Parnell_2026_Summer_Internship,
remote github.com/srmh0199/Parnell_2026_Summer_Internship).

Context: read CLAUDE.md and .claude/worklog.md in that repo first. In short: an
R/Quarto pipeline turns a DC305 event export (data/event_files/Monte Vista Dairy.csv)
into parquet intermediate files, and parnell_repro.qmd renders the reproductive KPI
report (insemination rate, CR, PR, abortion cohorts, % pregnant by 100/150/200 DIM).
The pipeline runs end to end; the pregnancy-rate output is suspected wrong.

Task for this session: validate the pregnancy-rate calculation in parnell_repro.qmd.
Compare the per-period PR numbers against DC305's own repro summary for the same
herd and date range, and against the conception-rate and insemination-rate tables in
the same report (PR should roughly track IR x CR). Find where the definitions diverge
— likely candidates are the eligibility filter (vwp = 50, the DNB/open/bred logic in
the `Eligibility` chunk) and the fact that `distinct(period_id, id_animal_lact)` in
the `Pregnancy Rate` chunk can keep the wrong row when a cow has several conceptions.

Constraints / things to know:
- Render with `quarto render parnell_repro.qmd` from the repo root; it reads the
  parquet files in data/intermediate_files directly and does not need step 3.
- To rebuild the parquet files, run step0_master_processing_my_data.R from the repo
  root. Do NOT set clean_slate <- TRUE; it deletes data/event_files.
- Rendering needs internet: fxn_load_os_fxns() sources functions from GitHub.
- A `BRED` event with R code P or A means the cow conceived; A is an abortion of a
  real conception, so it belongs in both the CR numerator and the abortion numerator.
- The most recent 2 periods are intentionally excluded from the trend graphs
  (pregnancy diagnosis lag) — that is not the bug.
```
