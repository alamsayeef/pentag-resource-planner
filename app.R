# PenTAG Resource Planner - Shiny entry point
# Run with: shiny::runApp()   (working directory = this folder)
# Shiny automatically sources every file in R/ before this runs.

library(shiny)
library(bslib)

db_init()

theme <- bs_theme(
  version = 5,
  primary = "#007d69",
  secondary = "#003c3c",
  base_font = font_collection("Outfit", "system-ui", "sans-serif")
)

ui <- page_navbar(
  title = "PenTAG Resource Planner",
  window_title = "PenTAG Resource Planner",
  theme = theme,
  header = tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  nav_panel("Overview", div(class = "page-wrap", mod_overview_ui("overview"))),
  nav_panel("Project",  div(class = "page-wrap", mod_project_ui("project"))),
  nav_panel("Employee", div(class = "page-wrap", mod_employee_ui("employee"))),
  nav_panel("Input",    div(class = "page-wrap", mod_input_ui("input")))
)

server <- function(input, output, session) {
  # Shared counter: every write bumps it, every tab re-reads the database.
  data_version <- reactiveVal(0)

  mod_overview_server("overview", data_version)
  mod_project_server("project", data_version)
  mod_employee_server("employee", data_version)
  mod_input_server("input", data_version)
}

shinyApp(ui, server)
