# Employee tab: pick employee + status -> their projects and a 12-month capacity heatmap.

mod_employee_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      class = "mb-3",
      card_body(
        div(
          class = "filter-bar",
          strong("Select employee:"),
          selectInput(ns("employee"), "Employee", choices = c("Select employee\u2026" = ""), width = "240px"),
          selectInput(ns("status"), "Project status", choices = c("Select status\u2026" = "", PLANNING_STATUSES), width = "200px")
        )
      ),
      uiOutput(ns("projects"))
    ),
    card(
      card_header(
        h5(class = "mb-0", "Capacity Heatmap"),
        p(class = "text-muted small mb-0",
          "12 months from the start of the first project with the selected status. Each project's hours are spread",
          "equally over the employee's working days (after public and personal holidays).",
          "Utilisation = load \u00f7 (FTE \u00d7 daily hours \u00d7 working days).")
      ),
      uiOutput(ns("heatmap"))
    )
  )
}

mod_employee_server <- function(id, data_version) {
  moduleServer(id, function(input, output, session) {

    observe({
      data_version()
      e <- repo_employees()
      ch <- c("Select employee\u2026" = "", id_choices(e))
      cur <- isolate(input$employee)
      updateSelectInput(session, "employee", choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else "")
    })

    plan <- reactive({
      data_version()
      req(input$employee, input$status)
      employee_planning(as.integer(input$employee), input$status)
    })

    hint <- function(msg) div(class = "text-center text-muted py-5", msg)

    output$projects <- renderUI({
      if (is.null(input$employee) || input$employee == "" || is.null(input$status) || input$status == "") {
        return(hint("Select an employee and a project status to see their projects."))
      }
      pl <- plan()
      if (is.null(pl)) return(hint("Employee not found."))
      if (is.null(pl$projects)) return(hint(paste0("No ", input$status, " projects for ", pl$employee$name, ".")))
      p <- pl$projects
      div(
        class = "table-responsive",
        tags$table(
          class = "table table-sm align-middle",
          tags$thead(tags$tr(lapply(c("Project", "Type", "Status", "PM", "PD", "Start", "End", "Role(s)", "Hours"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(p)), function(i) {
            tags$tr(
              tags$td(class = "fw-semibold", p$name[i]), tags$td(p$type_name[i] %or% "\u2014"),
              tags$td(span(class = paste("badge", paste0("bg-", tolower(p$status[i]))), p$status[i])),
              tags$td(p$pm_name[i] %or% "\u2014"), tags$td(p$pd_name[i] %or% "\u2014"),
              tags$td(fmt_date(p$start_date[i])), tags$td(fmt_date(p$end_date[i])),
              tags$td(p$roles[i]), tags$td(sprintf("%.1f", p$total_hours[i]))
            )
          }))
        )
      )
    })

    output$heatmap <- renderUI({
      if (is.null(input$employee) || input$employee == "" || is.null(input$status) || input$status == "") {
        return(hint("Select an employee and a status to see the heatmap."))
      }
      pl <- plan()
      if (is.null(pl) || is.null(pl$months)) return(hint(paste0("There are no ", input$status, " projects to build a timeline from.")))
      mw <- pl$months
      mon <- pl$monthly
      mx <- if (is.null(pl$matrix)) 1 else max(1, pl$matrix)

      project_rows <- NULL
      if (!is.null(pl$projects)) {
        project_rows <- lapply(seq_len(nrow(pl$projects)), function(i) {
          tags$tr(
            tags$td(class = "fw-semibold", pl$projects$name[i]),
            lapply(mw$key, function(k) {
              h <- pl$matrix[i, k]
              if (h > 0) {
                tags$td(class = "heat-cell", style = sprintf("background-color: rgba(0,168,126,%.2f)", 0.12 + 0.6 * h / mx), sprintf("%.0f", h))
              } else tags$td()
            })
          )
        })
      }

      tagList(
        div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm heatmap",
            tags$thead(tags$tr(tags$th("Project"), lapply(mw$label, tags$th))),
            tags$tbody(
              project_rows,
              tags$tr(class = "heat-total", tags$td("Total load (h)"), lapply(mon$load, function(x) tags$td(sprintf("%.0f", x)))),
              tags$tr(class = "heat-total", tags$td("Available (h)"), lapply(mon$available, function(x) tags$td(sprintf("%.0f", x)))),
              tags$tr(
                tags$td(class = "fw-semibold", "Utilisation"),
                lapply(seq_len(nrow(mon)), function(j) {
                  tags$td(class = paste("fw-semibold", paste0("rag-", mon$rag[j])),
                          if (mon$rag[j] == "UNAVAILABLE") "n/a" else sprintf("%.0f%%", mon$util[j]))
                })
              )
            )
          )
        ),
        div(
          class = "legend",
          span(class = "rag-GREEN", "\u2264 80%"), span(class = "rag-AMBER", "81\u2013100%"),
          span(class = "rag-RED", "> 100%"), span(class = "rag-UNAVAILABLE", "No available hours")
        )
      )
    })
  })
}
