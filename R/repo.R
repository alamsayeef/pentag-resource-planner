# Repository layer: every SQL query and write lives here.
# Modules call these functions and never write SQL themselves.

repo_update <- function(table, id, values, allowed) {
  values <- values[names(values) %in% allowed]
  if (length(values) == 0) return(invisible(0))
  sets <- paste0(names(values), " = ?", collapse = ", ")
  db_write(sprintf("UPDATE %s SET %s WHERE id = ?", table, sets), c(unname(values), list(id)))
}

# ---- config -----------------------------------------------------------------
repo_config_num <- function(key, default) {
  r <- db_read("SELECT value FROM app_config WHERE key = ?", list(key))
  if (nrow(r) == 0) return(default)
  suppressWarnings(as.numeric(r$value[1])) %or% default
}

# ---- roles ------------------------------------------------------------------
# PM first, PD second, then the rest by name
repo_roles <- function(active_only = TRUE) {
  sql <- "SELECT id, name, system_key, is_active FROM roles"
  if (active_only) sql <- paste(sql, "WHERE is_active = 1")
  r <- db_read(sql)
  key <- ifelse(is.na(r$system_key), "", r$system_key)
  rank <- ifelse(key == "PM", 0, ifelse(key == "PD", 1, 2))
  r <- r[order(rank, r$name), , drop = FALSE]
  rownames(r) <- NULL
  r
}

repo_role_add <- function(name) {
  db_tx(function(con) {
    ex <- DBI::dbGetQuery(con, "SELECT id, is_active FROM roles WHERE name = ?", params = list(name))
    if (nrow(ex) > 0 && ex$is_active[1] == 1) stop("UNIQUE: a role with that name already exists")
    if (nrow(ex) > 0) {
      DBI::dbExecute(con, "UPDATE roles SET is_active = 1 WHERE id = ?", params = list(ex$id[1]))
      rid <- ex$id[1]
    } else {
      DBI::dbExecute(con, "INSERT INTO roles (name) VALUES (?)", params = list(name))
      rid <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    }
    # every project type gets 50h for the new role (keeps edited hours if it existed)
    DBI::dbExecute(
      con,
      "INSERT OR IGNORE INTO role_hours (project_type_id, role_id, hours) SELECT id, ?, 50 FROM project_types",
      params = list(rid)
    )
  })
}

repo_role_rename <- function(id, name) repo_update("roles", id, list(name = name), "name")

# Deactivates the role (history kept). PM and PD can't be removed.
repo_role_remove <- function(id) {
  r <- db_read("SELECT system_key FROM roles WHERE id = ?", list(id))
  if (nrow(r) == 0) return(invisible(0))
  if (!is.na(r$system_key[1])) stop("The PM and PD roles can't be removed.")
  db_tx(function(con) {
    DBI::dbExecute(con, "UPDATE roles SET is_active = 0 WHERE id = ?", params = list(id))
    DBI::dbExecute(con, "UPDATE employees SET primary_role_id = NULL WHERE primary_role_id = ?", params = list(id))
  })
}

# ---- employees --------------------------------------------------------------
repo_employees <- function(all = FALSE) {
  sql <- paste(
    "SELECT e.id, e.name, e.email, e.fte, e.is_active, e.primary_role_id, r.name AS primary_role",
    "FROM employees e LEFT JOIN roles r ON r.id = e.primary_role_id",
    if (!all) "WHERE e.is_active = 1" else "",
    "ORDER BY e.name"
  )
  db_read(sql)
}

repo_employee_add <- function(name, email = NA_character_, fte = 1, primary_role_id = NA_integer_) {
  db_insert(
    "INSERT INTO employees (name, email, fte, primary_role_id) VALUES (?, ?, ?, ?)",
    list(name, email, fte, primary_role_id)
  )
}

repo_employee_update <- function(id, values) {
  repo_update("employees", id, values, c("name", "email", "fte", "is_active", "primary_role_id"))
}

# ---- personal leave ---------------------------------------------------------
repo_leave <- function(employee_id = NULL) {
  if (is.null(employee_id)) {
    db_read("SELECT id, employee_id, start_date, end_date FROM employee_leave ORDER BY start_date")
  } else {
    db_read(
      "SELECT id, employee_id, start_date, end_date FROM employee_leave WHERE employee_id = ? ORDER BY start_date",
      list(employee_id)
    )
  }
}

repo_leave_add <- function(employee_id, start_date, end_date) {
  db_write(
    "INSERT INTO employee_leave (employee_id, start_date, end_date) VALUES (?, ?, ?)",
    list(employee_id, format(start_date), format(end_date))
  )
}

repo_leave_delete <- function(id) db_write("DELETE FROM employee_leave WHERE id = ?", list(id))

# ---- public / closure holidays ----------------------------------------------
repo_holidays <- function() as.Date(db_read("SELECT date FROM org_holidays")$date)

# ---- project types and hours ------------------------------------------------
repo_types <- function() db_read("SELECT id, name FROM project_types ORDER BY name")

repo_role_hours <- function() db_read("SELECT project_type_id, role_id, hours FROM role_hours")

repo_type_add <- function(name) {
  db_tx(function(con) {
    DBI::dbExecute(con, "INSERT INTO project_types (name) VALUES (?)", params = list(name))
    tid <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    DBI::dbExecute(
      con,
      "INSERT INTO role_hours (project_type_id, role_id, hours) SELECT ?, id, 50 FROM roles WHERE is_active = 1",
      params = list(tid)
    )
    tid
  })
}

repo_type_rename <- function(id, name) repo_update("project_types", id, list(name = name), "name")

repo_set_hours <- function(type_id, role_id, hours) {
  db_write(
    paste(
      "INSERT INTO role_hours (project_type_id, role_id, hours) VALUES (?, ?, ?)",
      "ON CONFLICT(project_type_id, role_id) DO UPDATE SET hours = excluded.hours"
    ),
    list(type_id, role_id, hours)
  )
}

# ---- projects ---------------------------------------------------------------
repo_projects <- function() {
  db_read(paste(
    "SELECT p.id, p.code, p.name, p.project_type_id, pt.name AS type_name, p.status,",
    "p.start_date, p.end_date, p.pm_id, pm.name AS pm_name, p.pd_id, pd.name AS pd_name",
    "FROM projects p",
    "LEFT JOIN project_types pt ON pt.id = p.project_type_id",
    "LEFT JOIN employees pm ON pm.id = p.pm_id",
    "LEFT JOIN employees pd ON pd.id = p.pd_id",
    "ORDER BY p.id"
  ))
}

PROJECT_COLS <- c("code", "name", "project_type_id", "status", "start_date", "end_date", "pm_id", "pd_id")

repo_project_add <- function(values) {
  db_insert(
    sprintf("INSERT INTO projects (%s) VALUES (%s)", paste(PROJECT_COLS, collapse = ", "),
            paste(rep("?", length(PROJECT_COLS)), collapse = ", ")),
    unname(values[PROJECT_COLS])
  )
}

repo_project_update <- function(id, values) repo_update("projects", id, values, PROJECT_COLS)

repo_project_delete <- function(id) db_write("DELETE FROM projects WHERE id = ?", list(id))

# ---- role assignments (technical roles only; PM/PD live on the project) -----
repo_assignments <- function(project_id = NULL) {
  if (is.null(project_id)) {
    db_read("SELECT project_id, employee_id, role_id FROM project_assignments")
  } else {
    db_read("SELECT project_id, employee_id, role_id FROM project_assignments WHERE project_id = ?", list(project_id))
  }
}

repo_assign <- function(project_id, employee_id, role_id) {
  r <- db_read("SELECT system_key FROM roles WHERE id = ?", list(role_id))
  if (nrow(r) == 1 && !is.na(r$system_key[1])) stop("PM and PD are set on the project itself, not assigned here.")
  db_write(
    "INSERT OR IGNORE INTO project_assignments (project_id, employee_id, role_id) VALUES (?, ?, ?)",
    list(project_id, employee_id, role_id)
  )
}

repo_unassign <- function(project_id, employee_id, role_id) {
  db_write(
    "DELETE FROM project_assignments WHERE project_id = ? AND employee_id = ? AND role_id = ?",
    list(project_id, employee_id, role_id)
  )
}
