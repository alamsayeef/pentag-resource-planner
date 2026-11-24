source(testthat::test_path("..", "..", "R", "planning.R"))

test_that("working_days skips weekends, holidays and personal leave", {
  expect_length(working_days("2026-03-02", "2026-03-08"), 5)
  expect_length(working_days("2026-03-02", "2026-03-08", holidays = as.Date("2026-03-04")), 4)
  expect_length(working_days("2026-03-02", "2026-03-08", leave_days = as.Date(c("2026-03-02", "2026-03-03"))), 3)
  expect_length(working_days("2026-03-08", "2026-03-02"), 0)
})

test_that("leave ranges expand to calendar days", {
  lv <- data.frame(start_date = "2026-03-02", end_date = "2026-03-04", stringsAsFactors = FALSE)
  expect_length(leave_to_days(lv), 3)
  expect_length(leave_to_days(lv[0, ]), 0)
})

test_that("role hours are split equally and PM/PD get their own hours", {
  roles <- data.frame(id = 1:3, name = c("PM", "PD", "Role 1"), system_key = c("PM", "PD", NA), stringsAsFactors = FALSE)
  rh <- data.frame(project_type_id = 1L, role_id = 1:3, hours = c(10, 20, 60))
  p <- data.frame(project_type_id = 1L, pm_id = 5L, pd_id = 6L)
  asg <- data.frame(employee_id = c(5L, 7L), role_id = c(3L, 3L))
  ro <- build_roster(p, roles, rh, asg)
  tot <- tapply(ro$entries$hours, ro$entries$employee_id, sum)
  expect_equal(unname(tot["5"]), 10 + 30) # PM hours + half of Role 1
  expect_equal(unname(tot["6"]), 20)      # PD hours only
  expect_equal(unname(tot["7"]), 30)      # half of Role 1
})

test_that("unassigned roles and missing project type give no entries", {
  roles <- data.frame(id = 1L, name = "Role 1", system_key = NA_character_, stringsAsFactors = FALSE)
  rh <- data.frame(project_type_id = 1L, role_id = 1L, hours = 50)
  p <- data.frame(project_type_id = NA_integer_, pm_id = NA_integer_, pd_id = NA_integer_)
  ro <- build_roster(p, roles, rh, data.frame(employee_id = integer(), role_id = integer()))
  expect_equal(nrow(ro$entries), 0)
})

test_that("hours spread over working days and aggregate by month", {
  days <- working_days("2026-03-02", "2026-04-03")
  out <- spread_hours(100, days, c("2026-03", "2026-04"))
  expect_equal(sum(out), 100)
  expect_true(out[["2026-03"]] > out[["2026-04"]])
})

test_that("RAG thresholds", {
  expect_equal(rag_status(80, 100), "GREEN")
  expect_equal(rag_status(90, 100), "AMBER")
  expect_equal(rag_status(101, 100), "RED")
  expect_equal(rag_status(0, 0), "UNAVAILABLE")
})
