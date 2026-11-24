# PenTAG Resource Planner (R Shiny)

Resource and capacity planning for PenTAG, University of Exeter. R Shiny port of the React/Express app.

## Run it on Windows

1. Install **R** (4.2 or newer) and **RStudio**.
2. Extract this folder to `C:/pentag-resource-planner`.
3. Double-click `pentag-resource-planner.Rproj` (opens RStudio in the right folder).
4. In the console, once only: `source("scripts/install_packages.R")`
5. Start the app: `shiny::runApp()`

The first start creates `data/pentag.sqlite` with PM, PD, Role 1-4 and UK bank holidays.
For demo data: close the app, run `source("data-raw/seed_demo.R")`, then `shiny::runApp()` again.
For a clean start with real data, just don't run the seed.

## Folder tree

```
pentag-resource-planner/
├── app.R                  entry point: theme, navbar, wires the modules
├── R/                     auto-sourced by Shiny, in this order of dependence:
│   ├── utils.R              constants, formatting, inline HTML inputs
│   ├── db.R                 connection, schema creation, defaults  (swap here for Postgres)
│   ├── repo.R               ALL SQL: queries and writes
│   ├── planning.R           pure business logic (working days, hour splitting, RAG)
│   ├── service.R            repo + planning -> data for the Project/Employee tabs
│   ├── mod_overview.R       Overview tab module
│   ├── mod_project.R        Project tab module
│   ├── mod_employee.R       Employee tab module
│   └── mod_input.R          Input tab module
├── sql/schema.sql         table definitions
├── data/                  pentag.sqlite lives here (git-ignored)
├── data-raw/seed_demo.R   optional demo data loader
├── scripts/               install_packages.R, backup_db.R
├── tests/testthat/        unit tests for planning.R
└── www/custom.css         brand styling
```

Layers: **module (UI) -> service -> planning (pure) + repo -> db**. Modules never write SQL.

## Tabs

- **Overview** - status cards, cascading filters, project grid (select a row, then Edit / Delete; **+ Add project**), 12-month Gantt.
- **Project** - choose status then project; assign employees to roles; set PM and PD (single person each).
- **Employee** - choose employee and status; their projects and a 12-month capacity heatmap.
- **Input** - employees (name, email, primary role, status, FTE), personal holidays (calendar picker), roles (add/rename/remove), hours per role per project type (default 50).

## Planning rules

- Role hours come from the project type (Input tab) and are split equally between everyone holding the role on that project.
- PM and PD roles are held only by the project's PM / PD, who get those hours in full.
- A person's hours on a project are spread equally over their working days in the project window.
- Working days = Mon-Fri minus public/UoE closure days minus the person's personal holidays.
- Monthly available = FTE x daily hours (7.3) x working days. Utilisation = load / available. RAG: green <= 80%, amber <= 100%, red > 100%.
- Settings (`standard_hours_per_day`, `rag_green_max`, `rag_amber_max`) are rows in the `app_config` table.

## Data and backups

Everything is stored in `data/pentag.sqlite`. Back it up with `source("scripts/backup_db.R")`.
Do not commit the database if it holds real staff data (it is git-ignored by default).
Set the environment variable `PENTAG_DB` to use a different file path.

## Tests

`testthat::test_dir("tests/testthat")`

## Known limits

- SQLite is fine for one or a few users; for many simultaneous editors move to Postgres/Azure SQL (change `db.R`).
- No login. Host behind Posit Connect / Shiny Server with authentication before sharing.
- Public/UoE holidays for 2026-2027 are preloaded; add later years in `default_holidays()` in `R/db.R` (existing databases need the new dates inserted into `org_holidays`).
