# Database connection and one-off set-up. SQLite file lives in data/.
# To move to Postgres/Azure SQL later, only this file and repo.R need to change.

db_path <- function() Sys.getenv("PENTAG_DB", unset = file.path("data", "pentag.sqlite"))

db_connect <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

db_read <- function(sql, params = NULL) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql, params = params)
}

db_write <- function(sql, params = NULL) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sql, params = params)
}

# Insert one row and return its new id
db_insert <- function(sql, params = NULL) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sql, params = params)
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
}

# Run several statements atomically: db_tx(function(con) { ... })
db_tx <- function(fn) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, fn(con))
}

# Create tables if missing and add defaults (config, PM/PD/Role 1-4, UK holidays)
db_init <- function() {
  dir.create(dirname(db_path()), showWarnings = FALSE, recursive = TRUE)
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))

  lines <- readLines(file.path("sql", "schema.sql"), warn = FALSE)
  lines <- grep("^\\s*--", lines, invert = TRUE, value = TRUE)
  stmts <- trimws(strsplit(paste(lines, collapse = "\n"), ";", fixed = TRUE)[[1]])
  for (s in stmts[nzchar(stmts)]) DBI::dbExecute(con, s)

  cfg <- list(
    c("standard_hours_per_day", "7.3"),
    c("rag_green_max", "80"),
    c("rag_amber_max", "100")
  )
  for (kv in cfg) {
    DBI::dbExecute(con, "INSERT OR IGNORE INTO app_config (key, value) VALUES (?, ?)", params = list(kv[1], kv[2]))
  }

  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM roles")$n == 0) {
    roles <- list(
      c("PM", "PM"), c("PD", "PD"),
      c("Role 1", NA), c("Role 2", NA), c("Role 3", NA), c("Role 4", NA)
    )
    for (r in roles) {
      DBI::dbExecute(con, "INSERT INTO roles (name, system_key) VALUES (?, ?)", params = list(r[1], r[2]))
    }
  }

  hol <- default_holidays()
  for (i in seq_len(nrow(hol))) {
    DBI::dbExecute(
      con, "INSERT OR IGNORE INTO org_holidays (name, date, holiday_type) VALUES (?, ?, ?)",
      params = list(hol$name[i], hol$date[i], hol$type[i])
    )
  }
  invisible(TRUE)
}

# England and Wales bank holidays and UoE closure days (edit/extend as needed)
default_holidays <- function() {
  d <- function(name, date, type = "Bank Holiday") data.frame(name = name, date = date, type = type, stringsAsFactors = FALSE)
  rbind(
    d("New Year's Day", "2026-01-01"), d("Good Friday", "2026-04-03"), d("Easter Monday", "2026-04-06"),
    d("Early May Bank Holiday", "2026-05-04"), d("Spring Bank Holiday", "2026-05-25"),
    d("Summer Bank Holiday", "2026-08-31"), d("Christmas Day", "2026-12-25"),
    d("Boxing Day (substitute)", "2026-12-28"),
    d("UoE Closure 1", "2026-12-29", "UoE Closure"), d("UoE Closure 2", "2026-12-30", "UoE Closure"),
    d("UoE Closure 3", "2026-12-31", "UoE Closure"),
    d("New Year's Day", "2027-01-01"), d("Good Friday", "2027-03-26"), d("Easter Monday", "2027-03-29"),
    d("Early May Bank Holiday", "2027-05-03"), d("Spring Bank Holiday", "2027-05-31"),
    d("Summer Bank Holiday", "2027-08-30"), d("Christmas Day (substitute)", "2027-12-27"),
    d("Boxing Day (substitute)", "2027-12-28")
  )
}
