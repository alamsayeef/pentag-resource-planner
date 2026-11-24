# Pure planning logic: data frames in, data frames out. No database, no Shiny.
# This is the part that is unit-tested (tests/testthat/test-planning.R).

empty_dates <- function() as.Date(character())

days_between <- function(from, to) {
  from <- as.Date(from)
  to <- as.Date(to)
  if (length(from) != 1 || length(to) != 1 || is.na(from) || is.na(to) || to < from) return(empty_dates())
  seq(from, to, by = "day")
}

# Mon-Fri minus public/closure holidays minus personal leave days
working_days <- function(from, to, holidays = empty_dates(), leave_days = empty_dates()) {
  d <- days_between(from, to)
  if (!length(d)) return(d)
  weekday <- as.integer(format(d, "%u"))
  d[weekday <= 5 & !(d %in% holidays) & !(d %in% leave_days)]
}

# Every calendar day covered by a set of leave ranges
leave_to_days <- function(leave) {
  if (is.null(leave) || nrow(leave) == 0) return(empty_dates())
  out <- do.call(c, lapply(seq_len(nrow(leave)), function(i) days_between(leave$start_date[i], leave$end_date[i])))
  if (is.null(out)) empty_dates() else unique(out)
}

# Who holds which role on a project, and how many hours each person gets.
#  - role hours come from the project type
#  - PM / PD roles (system_key) are held by the project's PM / PD only
#  - any other role is split equally between everyone assigned to it
build_roster <- function(project, roles, role_hours, assignments) {
  blocks <- lapply(seq_len(nrow(roles)), function(i) {
    r <- roles[i, , drop = FALSE]
    hours <- 0
    if (!is.na(project$project_type_id)) {
      h <- role_hours$hours[role_hours$project_type_id == project$project_type_id & role_hours$role_id == r$id]
      if (length(h) == 1 && !is.na(h)) hours <- h
    }
    key <- r$system_key
    ids <- if (identical(key, "PM")) {
      project$pm_id
    } else if (identical(key, "PD")) {
      project$pd_id
    } else {
      assignments$employee_id[assignments$role_id == r$id]
    }
    ids <- unique(ids[!is.na(ids)])
    list(
      role_id = r$id, role_name = r$name, system_key = key, role_hours = hours,
      employee_ids = ids, share = if (length(ids)) hours / length(ids) else 0
    )
  })
  parts <- lapply(blocks, function(b) {
    if (length(b$employee_ids) == 0) return(NULL)
    data.frame(employee_id = b$employee_ids, role_name = b$role_name, hours = b$share, stringsAsFactors = FALSE)
  })
  entries <- do.call(rbind, parts)
  if (is.null(entries)) {
    entries <- data.frame(employee_id = integer(), role_name = character(), hours = numeric(), stringsAsFactors = FALSE)
  }
  list(blocks = blocks, entries = entries)
}

available_hours <- function(fte, hours_per_day, n_days) round(fte * hours_per_day * n_days, 2)

rag_status <- function(util, available, green_max = 80, amber_max = 100) {
  if (available <= 0) "UNAVAILABLE" else if (util > amber_max) "RED" else if (util > green_max) "AMBER" else "GREEN"
}

# 12 calendar months starting at the month of `earliest`
month_window <- function(earliest, n = 12) {
  first <- as.Date(format(as.Date(earliest), "%Y-%m-01"))
  starts <- seq(first, by = "month", length.out = n)
  ends <- seq(first, by = "month", length.out = n + 1)[-1] - 1
  data.frame(key = format(starts, "%Y-%m"), label = format(starts, "%b %y"),
             start = starts, end = ends, stringsAsFactors = FALSE)
}

# Spread `total` hours equally over `days`, then sum per month key
spread_hours <- function(total, days, keys) {
  out <- stats::setNames(numeric(length(keys)), keys)
  if (!length(days) || total == 0) return(out)
  agg <- tapply(rep(total / length(days), length(days)), format(days, "%Y-%m"), sum)
  agg <- agg[names(agg) %in% keys]
  out[names(agg)] <- as.numeric(agg)
  out
}
