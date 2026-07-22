# Parnell 2026 Summer Internship — Dairy Reproductive KPIs

R / Quarto project that turns raw DC305 herd-management event exports into
reproductive performance KPIs for a dairy, and renders them as a self-contained
HTML report. Built during the Parnell 2026 summer internship (Sarah Hills, with
Nora Schrag / Animal Clinic LLC). The processing pipeline is a fork/adaptation of
the shared template at
<https://github.com/nora-lvr/DairyHealthDataProcessingSimple>.

The project question (see `Parnell Summer Project 2026.qmd`): *what are the repro
KPIs of an individual dairy, and how do they compare to industry standards?* —
CR, PR, abortion rate, % pregnant by 100/150/200 DIM, and eventually protocol
comparison and cross-herd (anonymized) benchmarking.

## How it works

Run everything from the repo root (paths are relative or `here::here()`).

1. **`step0_master_processing_my_data.R`** — the only file you normally edit to
   configure a run. Loads packages via `pacman`, sources `functions/`, sets the
   knobs (animal-ID function, denominator time periods `c(21, 30, 90, 365)`,
   DIM/age grouping cuts, parsing and location functions, `clean_slate`), then
   sources steps 1–2 and renders the step 3 denominator `.qmd`s.
   `clean_slate <- TRUE` **deletes** everything in `data/event_files` and
   `data/intermediate_files` — leave it `FALSE` unless you mean it.
2. **`step1_read_in_data.R`** — reads every CSV in `data/event_files/` as all-character,
   standardizes column names, assigns `id_animal` / `id_animal_lact`, parses dates
   (`mdy`), assigns `event_type` / `location_event` / disease-lesion parsing, builds
   the `lact_group*` variables. Writes `events_all_columns.parquet` (everything, for
   debugging) and `events_formatted.parquet` (the analysis file), plus QC files and
   editable templates in `data/template_files/`.
3. **`step2_create_intermediate_files.R`** — collapses events into
   `animals.parquet` (one row per animal) and `animal_lactations.parquet` (one row
   per animal-lactation, with `date_fresh`, `date_dry`, `date_archive`,
   `date_next_fresh`, `date_dim30…date_dim305`), plus `events_parsed.parquet`.
4. **Step 3 `.qmd`s** — build denominator files by rolling time period, by calendar
   time, and by lact/DIM/season. Rendered HTML lands in `reports/`.
5. **`parnell_repro.qmd`** — the actual deliverable. Reads the three parquet files
   directly (it does **not** require step 3 denominators) and produces
   `parnell_repro.html`. Render with `quarto render parnell_repro.qmd`; the farm
   name comes from the `event` param (default `"Monte Vista"`).
6. **`ParnellRepro/app.R`** — an interactive Shiny version of `parnell_repro.qmd`.
   Reads the same three parquet files (via `here::here()`, so it resolves the project
   root even from the subfolder) and reproduces the report's KPIs as tabs, with
   controls for the voluntary waiting period, number of 21-day periods, plotted
   lactation groups, and how many recent periods to drop from the trend graphs. Run
   with `shiny::runApp("ParnellRepro")` from the project root. Unlike the report it
   does **not** source the GitHub "os" functions, so it runs offline.

`functions/` holds `fxn_*.R` helpers; most come from the shared template.
Sarah-authored ones are `fxn_denos_sarah.R` (yearly herd denominators) and
`fxn_denominators_monthly.R` (monthly denominators, 2-year lookback).
`fxn_load_os_fxns.R` pulls Gerard's lameness functions from the
`Dairy-Cow-Foot-Doc/os_functions` GitHub repo **at render time** — renders need
internet and are subject to GitHub API rate limits.

## KPI definitions used in `parnell_repro.qmd`

These are the definitions the report currently encodes — worth checking against
what the producer expects before publishing numbers.

- **Periods** — 24 rolling 21-day periods counting *backwards* from the most recent
  event date in the data (`date_max_pull`), not calendar months.
- **Eligible** — lactating (`lact_number > 0`), freshened before the period end,
  not archived, not dry, past a voluntary waiting period of **`vwp <- 50` days**,
  not already bred-and-still-pregnant, and not DNB.
- **Conception / pregnancy** — a `BRED` event whose `R` code is `P` or `A`. `A`
  (aborted) counts as a conception because the cow *did* conceive; abortion is then
  measured separately as `R == "A"` over `P + A`, grouped by the month the cow was
  bred (cohort, not calendar month of the abortion).
- **Rebreed** — `R == "R"` on a `BRED` event, reported monthly.
- **% pregnant by 100/150/200 DIM** — denominator is all lactations fresh at least
  100/150/200 days before `date_max_pull`, grouped by calving month; benchmarks
  drawn as dashed lines at **50% / 75% / 90%**.
- **Trailing periods are dropped from the trend graphs** (`filter(period_start < …[2])`,
  and the 3/5/7-month cutoffs on the DIM-milestone cohorts). This is deliberate:
  recent cows haven't had a pregnancy diagnosis yet, so including them makes the
  most recent points collapse toward zero and look like a performance crash.

## Key facts

- Remote: `https://github.com/srmh0199/Parnell_2026_Summer_Internship.git` (branch `main`).
- Upstream template: <https://github.com/nora-lvr/DairyHealthDataProcessingSimple>.
- Real herd data is **committed to this repo**: `data/event_files/Monte Vista Dairy.csv`
  (~62 MB), plus the derived `.parquet` files and rendered HTML. That makes the repo
  self-contained and reproducible, but it means anyone with repo access has the
  dairy's raw records — confirm the repo's visibility is appropriate before sharing it.
- `data/.gitignore` ignores a different DC305 export, so the ignore rules and what's
  actually tracked have drifted apart.

## Status / open items

- Working and rendering: insemination rate, rebreed rate, conception rate, pregnancy
  rate (all by lactation group), abortion cohort table + graph, and the combined
  100/150/200 DIM milestone graph.
- **Pregnancy-rate graph still looks off** (noted in commit `c364545`) — the DIM
  milestone work is "done (maybe?)". Not yet validated against DC305's own repro
  summary, which is the obvious next check.
- **`ParnellRepro/app.R` is a working Shiny app** (rebuilt 2026-07-22 from the old
  broken scaffold, which referenced undefined variables `df_herd` / `lactation_group`
  / `pregnancy_status` / `days_in_milk`). It ports `parnell_repro.qmd`'s calculations
  verbatim and was verified end to end with `shiny::testServer` and a live browser
  render. The anonymized *cross-herd* comparison app — the original intent of this
  folder — is still unstarted; this is the single-herd report as an app.
- **`monthly_report.qmd` does not render.** Line 92 calls `date_min_pull()` as a
  function when it's a Date, and the logo path (`milestones_dairy/images/ac logo.jpeg`)
  doesn't exist in this repo.
- `parnell_repro.qmd` points its author image at `images/parnell.jpeg`, but the repo
  has `images/parnell.png` — the header image is broken in the rendered HTML.
- Two `.Rproj` files exist (`Parnell-2026-Repro-Project.Rproj` and
  `Parnell_2026_Summer_Internship.Rproj`); one is a leftover and should be deleted.
- Protocol comparison (chalk vs. tail vs. other, timing of breeding as a protocol
  marker) and the anonymized cross-herd comparison are still unstarted.

See `.claude/worklog.md` for the dated history of what changed and why.
