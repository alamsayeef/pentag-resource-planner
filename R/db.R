# Database connection and one-off set-up supporting both SQLite (local) and PostgreSQL/Supabase (shinyapps.io).
# Configure Supabase via environment variables: DB_HOST, DB_NAME, DB_USER, DB_PASSWORD, DB_PORT.
# If DB_HOST is set, it connects to Supabase PostgreSQL using RPostgres; otherwise it falls back to SQLite.

db_path <- function() Sys.getenv("PENTAG_DB", unset = file.path("data", "pentag.sqlite"))

is_postgres <- function() {
  nzchar(Sys.getenv("DB_HOST", ""))
}

db_connect <- function() {
  if (is_postgres()) {
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("DB_HOST", "myzqbqgrqsbqpuhtgvxx.supabase.co"),
      dbname = Sys.getenv("DB_NAME", "postgres"),
      user = Sys.getenv("DB_USER", "postgres"),
      password = Sys.getenv("DB_PASSWORD", ""),
      port = as.integer(Sys.getenv("DB_PORT", "5432"))
    )
    con
  } else {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path())
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
    con
  }
}

db_read <- function(sql, params = NULL) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  if (is_postgres()) {
    # Replace SQLite specific placeholders '?' with PostgreSQL '$1', '$2', etc. if needed, or use DBI parameterized queries if supported.
    # RPostgres supports DBI parameterized queries with '?' or '$n'. Let's ensure compatibility or parameter formatting.
    # Actually RPostgres/DBI standard supports parameterised queries with `params` list.
  }
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
  if (is_postgres()) {
    # For Postgres, standard INSERT often needs RETURNING id
    # Let's inspect if sql has RETURNING. If not, append RETURNING id.
    if (!grepl("RETURNING", sql, ignore.case = TRUE)) {
      sql <- paste(sql, "RETURNING id")
    }
    res <- DBI::dbGetQuery(con, sql, params = params)
    res$id[1]
  } else {
    DBI::dbExecute(con, sql, params = params)
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
  }
}

# Run several statements atomically: db_tx(function(con) { ... })
db_tx <- function(fn) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, fn(con))
}

# Create tables if missing and add defaults (config, PM/PD/Role 1-4, UK holidays)
db_init <- function() {
  if (!is_postgres()) {
    dir.create(dirname(db_path()), showWarnings = FALSE, recursive = TRUE)
  }
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))

  schema_file <- if (is_postgres()) file.path("sql", "postgres_schema.sql") else file.path("sql", "schema.sql")
  lines <- readLines(schema_file, warn = FALSE)
  lines <- grep("^\\s*--", lines, invert = TRUE, value = TRUE)
  stmts <- trimws(strsplit(paste(lines, collapse = "\n"), ";", fixed = TRUE)[[1]])
  for (s in stmts[nzchar(stmts)]) {
    tryCatch({
      DBI::dbExecute(con, s)
    }, error = function(e) {
      # Ignore "already exists" errors during init
    })
  }

  cfg <- list(
    c("standard_hours_per_day", "7.3"),
    c("rag_green_max", "80"),
    c("rag_amber_max", "100")
  )
  for (kv in cfg) {
    if (is_postgres()) {
      DBI::dbExecute(con, "INSERT INTO app_config (key, value) VALUES (?, ?) ON CONFLICT (key) DO NOTHING", params = list(kv[1], kv[2]))
    } else {
      DBI::dbExecute(con, "INSERT OR IGNORE INTO app_config (key, value) VALUES (?, ?)", params = list(kv[1], kv[2]))
    }
  }

  if (DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM roles")$n == 0) {
    roles <- list(
      c("PM", "PM"), c("PD", "PD"),
      c("Role 1", NA), c("Role 2", NA), c("Role 3", NA), c("Role 4", NA)
    )
    for (r in roles) {
      if (is_postgres()) {
        DBI::dbExecute(con, "INSERT INTO roles (name, system_key) VALUES (?, ?) ON CONFLICT (name) DO NOTHING", params = list(r[1], r[2]))
      } else {
        DBI::dbExecute(con, "INSERT INTO roles (name, system_key) VALUES (?, ?)", params = list(r[1], r[2]))
      }
    }
  }

  hol <- default_holidays()
  for (i in seq_len(nrow(hol))) {
    if (is_postgres()) {
      DBI::dbExecute(
        con, "INSERT INTO org_holidays (name, date, holiday_type) VALUES (?, ?, ?) ON CONFLICT (date) DO NOTHING",
        params = list(hol$name[i], hol$date[i], hol$type[i])
      )
    } else {
      DBI::dbExecute(
        con, "INSERT OR IGNORE INTO org_holidays (name, date, holiday_type) VALUES (?, ?, ?)",
        params = list(hol$name[i], hol$date[i], hol$type[i])
      )
    }
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
