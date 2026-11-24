# One-off: install everything the app needs.  source("scripts/install_packages.R")
pkgs <- c("shiny", "bslib", "DBI", "RSQLite", "RPostgres", "DT", "ggplot2", "htmltools", "testthat", "openxlsx", "readxl")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
message("All packages installed.")
