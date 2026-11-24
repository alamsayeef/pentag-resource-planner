# Project tab: pick status -> project, see details, assign people to roles.

mod_project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      class = "mb-3",
      card_body(
        div(
          class = "filter-bar",
          strong("Select project:"),
          selectInput(ns("status"), "Status", choices = c("Select status\u2026" = "", PLANNING_STATUSES), width = "200px"),
          selectInput(ns("project"), "Project", choices = c("Select a status first" = ""), width = "300px")
        )
      )
    ),
    uiOutput(ns("detail"))
  )
}

mod_project_server <- function(id, data_version) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bump <- function() data_version(data_version() + 1)

    # project dropdown follows the chosen status (and refreshes after edits elsewhere)
    observe({
      data_version()
      st <- input$status
      if (is.null(st) || st == "") {
        updateSelectInput(session, "project", choices = c("Select a status first" = ""))
        return()
      }
      p <- repo_projects()
      p <- p[p$status == st, , drop = FALSE]
      p <- p[order(p$name), , drop = FALSE]
      labels <- ifelse(is.na(p$code) | p$code == "", p$name, paste0(p$code, " \u2014 ", p$name))
      ch <- c("Select project\u2026" = "", stats::setNames(as.character(p$id), labels))
      cur <- isolate(input$project)
      updateSelectInput(session, "project", choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else "")
    })

    pid <- reactive({
      req(input$project)
      as.integer(input$project)
    })

    observeEvent(input$assign, {
      a <- input$assign
      if (!nzchar(a$value)) return()
      safe_run(repo_assign(pid(), as.integer(a$value), as.integer(a$role)))
      bump()
    })
    observeEvent(input$unassign, {
      a <- input$unassign
      safe_run(repo_unassign(pid(), as.integer(a$emp), as.integer(a$role)))
      bump()
    })
    observeEvent(input$setlead, {
      a <- input$setlead
      col <- if (identical(a$key, "PM")) "pm_id" else "pd_id"
      safe_run(repo_project_update(pid(), stats::setNames(list(as_id(a$value)), col)))
      bump()
    })

    output$detail <- renderUI({
      data_version()
      if (is.null(input$project) || input$project == "") {
        return(card(card_body(class = "text-center text-muted py-5", "Select a status and a project to see its details.")))
      }
      d <- project_detail(as.integer(input$project))
      if (is.null(d)) return(NULL)
      p <- d$project
      rows <- d$rows
      emps <- repo_employees()
      all_people <- id_choices(emps)
      first <- !duplicated(rows$role_id)

      fact <- function(label, value) div(p(class = "text-muted small mb-0", label), p(class = "fw-semibold", value %or% "\u2014"))

      load_cell <- function(r) {
        if (is.na(r$available) || r$available <= 0) return(tags$td("\u2014"))
        pct <- r$total / r$available * 100
        cls <- if (pct > 100) "text-danger" else if (pct > 80) "text-warning" else "text-success"
        tags$td(class = paste("fw-semibold", cls), sprintf("%.0f%%", pct))
      }
      num_cells <- function(r) {
        if (is.na(r$employee_id)) return(list(tags$td(colspan = 5)))
        list(
          tags$td(sprintf("%.1f", r$fte)), tags$td(sprintf("%.1f", r$hours)),
          tags$td(sprintf("%.1f", r$total)), tags$td(sprintf("%.1f", r$available)), load_cell(r)
        )
      }

      body <- lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, , drop = FALSE]
        lab <- tags$td(class = "fw-semibold", if (first[i]) r$role_name)
        hrs <- tags$td(if (first[i]) sprintf("%.1f", r$role_hours))
        if (r$kind == "lead") {
          tags$tr(
            lab, hrs,
            tags$td(inline_select(ns("setlead"), sprintf("key: '%s'", r$system_key), all_people,
                                  selected = as.character(r$employee_id %or% ""), placeholder = "\u2014 Not set \u2014")),
            num_cells(r), tags$td()
          )
        } else if (r$kind == "person") {
          tags$tr(
            lab, hrs, tags$td(r$name), num_cells(r),
            tags$td(remove_link(ns("unassign"), sprintf("role: %d, emp: %d", r$role_id, r$employee_id), "Remove from role"))
          )
        } else {
          taken <- rows$employee_id[rows$role_id == r$role_id & rows$kind == "person"]
          cand <- emps[!(emps$id %in% taken), , drop = FALSE]
          none_yet <- !any(rows$role_id == r$role_id & rows$kind == "person")
          tags$tr(
            lab, hrs,
            tags$td(colspan = 7, inline_select(
              ns("assign"), sprintf("role: %d", r$role_id), id_choices(cand),
              placeholder = if (none_yet) "Unassigned \u2014 add employee\u2026" else "+ Add employee\u2026"
            ))
          )
        }
      })

      tagList(
        card(
          class = "mb-3",
          card_header(h5(class = "mb-0", p$name, span(class = paste("badge ms-2", paste0("bg-", tolower(p$status))), p$status))),
          card_body(div(
            class = "fact-grid",
            fact("Project ID", p$code), fact("Project type", p$type_name),
            fact("Start date", fmt_date(p$start_date)), fact("End date", fmt_date(p$end_date)),
            fact("PM", p$pm_name), fact("PD", p$pd_name)
          ))
        ),
        card(
          card_header(
            h5(class = "mb-0", "Roles & Hours"),
            p(class = "text-muted small mb-0",
              "Pick people from the dropdown under each role; \u00d7 removes them. PM and PD are single-person roles.",
              "Role hours come from the project type and are split equally between holders.",
              "Available hours = FTE \u00d7 daily hours \u00d7 working days in the project window",
              "(minus public and personal holidays).")
          ),
          div(
            class = "table-responsive",
            tags$table(
              class = "table table-sm align-middle roles-table",
              tags$thead(tags$tr(lapply(
                c("Role", "Role hours", "Employee", "FTE", "Hours on this role", "Total on project", "Available hours", "Load", ""),
                tags$th
              ))),
              tags$tbody(body)
            )
          )
        )
      )
    })
  })
}
