# Optional: load demo data into a fresh database.
# Run from the project root:  source("data-raw/seed_demo.R")
# WARNING: only run on an empty database (delete data/pentag.sqlite first to start clean).

for (f in c("utils.R", "db.R", "repo.R")) source(file.path("R", f))
db_init()

if (nrow(repo_employees(all = TRUE)) > 0) stop("Database already has employees - delete data/pentag.sqlite first.")

emp <- c("Employee 1", "Employee 2", "Employee 3", "Employee 4", "Employee 5", "Employee 6")
fte <- c(1.0, 0.8, 1.0, 0.6, 1.0, 0.5)
ids <- vapply(seq_along(emp), function(i) repo_employee_add(emp[i], paste0("employee", i, "@example.ac.uk"), fte[i]), numeric(1))

types <- vapply(c("Project Type 1", "Project Type 2", "Project Type 3"), repo_type_add, numeric(1))

p <- function(i, code, type, status, start, end, pm, pd) {
  repo_project_add(list(
    code = code, name = paste("Project Name", i), project_type_id = unname(types[type]), status = status,
    start_date = start, end_date = end, pm_id = ids[pm], pd_id = ids[pd]
  ))
}
p(1,  "P001", 1, "Ongoing",   "2026-09-01", "2027-06-30", 1, 3)
p(2,  "P002", 1, "Ongoing",   "2026-10-15", "2027-08-15", 2, 5)
p(3,  "P003", 2, "Ongoing",   "2026-11-01", "2027-01-31", 3, 1)
p(4,  "P004", 3, "Incoming",  "2027-01-15", "2027-07-15", 5, 1)
p(5,  "P005", 1, "Incoming",  "2027-02-01", "2027-11-30", 1, 2)
p(6,  "P006", 2, "Completed", "2026-03-01", "2026-05-31", 2, 3)
p(7,  "P007", 3, "Completed", "2026-01-10", "2026-07-10", 3, 5)
p(8,  "P008", 1, "Removed",   "2026-06-01", "2027-03-31", 5, 2)
p(9,  "P009", 3, "Removed",   "2025-09-01", "2026-03-01", 1, 5)
p(10, "P010", 2, "Ongoing",   "2026-08-01", "2026-12-31", 4, 3)
p(11, "P011", 3, "Incoming",  "2027-03-01", "2027-09-30", 3, 4)
p(12, "P012", 1, "Completed", "2025-11-01", "2026-08-30", 4, 1)

repo_leave_add(ids[1], "2026-12-21", "2026-12-24")
repo_leave_add(ids[3], "2027-02-15", "2027-02-19")
message("Demo data loaded. Run shiny::runApp()")
