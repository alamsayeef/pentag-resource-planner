# Service layer: reads from the repository, applies planning.R, returns
# ready-to-display structures for the Project and Employee tabs.

project_detail <- function(project_id) {
  projects <- repo_projects()
  p <- projects[projects$id == project_id, , drop = FALSE]
  if (nrow(p) == 0) return(NULL)

  roles <- repo_roles()
  ro <- build_roster(p, roles, repo_role_hours(), repo_assignments(project_id))
  emps <- repo_employees(all = TRUE)
  hol <- repo_holidays()
  hpd <- repo_config_num("standard_hours_per_day", 7.3)
  totals <- tapply(ro$entries$hours, ro$entries$employee_id, sum)

  person <- function(b, kind, eid) {
    base <- data.frame(
      role_id = b$role_id, role_name = b$role_name, system_key = b$system_key, role_hours = b$role_hours,
      kind = kind, employee_id = NA_integer_, name = NA_character_, fte = NA_real_, hours = NA_real_,
      total = NA_real_, available = NA_real_, stringsAsFactors = FALSE
    )
    if (is.na(eid)) return(base)
    e <- emps[emps$id == eid, , drop = FALSE]
    days <- length(working_days(p$start_date, p$end_date, hol, leave_to_days(repo_leave(eid))))
    key <- as.character(eid)
    base$employee_id <- as.integer(eid)
    base$name <- e$name
    base$fte <- e$fte
    base$hours <- round(b$share, 2)
    base$total <- round(if (key %in% names(totals)) unname(totals[key]) else 0, 2)
    base$available <- available_hours(e$fte, hpd, days)
    base
  }

  rows <- list()
  for (b in ro$blocks) {
    if (!is.na(b$system_key)) {
      eid <- if (b$system_key == "PM") p$pm_id else p$pd_id
      rows[[length(rows) + 1]] <- person(b, "lead", eid)
    } else {
      for (eid in b$employee_ids) rows[[length(rows) + 1]] <- person(b, "person", eid)
      rows[[length(rows) + 1]] <- person(b, "add", NA_integer_)
    }
  }
  list(project = p, rows = do.call(rbind, rows))
}

employee_planning <- function(employee_id, status) {
  emp <- repo_employees(all = TRUE)
  emp <- emp[emp$id == employee_id, , drop = FALSE]
  if (nrow(emp) == 0) return(NULL)

  all_projects <- repo_projects()
  sp <- all_projects[all_projects$status == status, , drop = FALSE]
  if (nrow(sp) == 0) return(list(employee = emp, months = NULL, projects = NULL, matrix = NULL, monthly = NULL))

  roles <- repo_roles()
  rh <- repo_role_hours()
  asg <- repo_assignments()
  hol <- repo_holidays()
  hpd <- repo_config_num("standard_hours_per_day", 7.3)
  green <- repo_config_num("rag_green_max", 80)
  amber <- repo_config_num("rag_amber_max", 100)
  leave <- leave_to_days(repo_leave(employee_id))

  mw <- month_window(min(as.Date(sp$start_date)))
  rows <- list()
  mat <- list()
  for (i in seq_len(nrow(sp))) {
    p <- sp[i, , drop = FALSE]
    ro <- build_roster(p, roles, rh, asg[asg$project_id == p$id, , drop = FALSE])
    mine <- ro$entries[ro$entries$employee_id == employee_id, , drop = FALSE]
    if (nrow(mine) == 0) next
    total <- sum(mine$hours)
    days <- working_days(p$start_date, p$end_date, hol, leave)
    mat[[length(mat) + 1]] <- spread_hours(total, days, mw$key)
    rows[[length(rows) + 1]] <- data.frame(
      id = p$id, name = p$name, type_name = p$type_name, status = p$status,
      pm_name = p$pm_name, pd_name = p$pd_name, start_date = p$start_date, end_date = p$end_date,
      roles = paste(mine$role_name, collapse = ", "), total_hours = round(total, 2),
      stringsAsFactors = FALSE
    )
  }
  projects <- if (length(rows)) do.call(rbind, rows) else NULL
  mat <- if (length(mat)) do.call(rbind, mat) else NULL

  monthly <- do.call(rbind, lapply(seq_len(nrow(mw)), function(j) {
    n <- length(working_days(mw$start[j], mw$end[j], hol, leave))
    avail <- available_hours(emp$fte, hpd, n)
    load <- if (is.null(mat)) 0 else round(sum(mat[, mw$key[j]]), 2)
    util <- if (avail > 0) round(load / avail * 100, 1) else 0
    data.frame(key = mw$key[j], available = avail, load = load, util = util,
               rag = rag_status(util, avail, green, amber), stringsAsFactors = FALSE)
  }))

  list(employee = emp, months = mw, projects = projects, matrix = mat, monthly = monthly)
}
