# Overview tab: status cards, cascading filters, editable project grid, Gantt chart.

mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    card(
      class = "mb-3",
      card_body(
        div(
          class = "filter-bar",
          strong("Filters:"),
          selectInput(ns("status"), "Status", choices = c("All", STATUSES), width = "170px"),
          selectInput(ns("type"), "Type", choices = "All", width = "170px"),
          selectInput(ns("pm"), "PM", choices = "All", width = "170px"),
          selectInput(ns("pd"), "PD", choices = "All", width = "170px"),
          actionLink(ns("reset"), "Reset all")
        )
      )
    ),
    card(
      class = "mb-3",
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          h5(class = "mb-0", "Projects"),
          div(
            class = "d-flex align-items-center gap-2",
            actionButton(ns("add"), "+ Add project", class = "btn-primary btn-sm btn-single-line"),
            actionButton(ns("edit"), "Edit selected", class = "btn-outline-secondary btn-sm btn-single-line"),
            actionButton(ns("delete"), "Delete selected", class = "btn-outline-danger btn-sm btn-single-line")
          )
        )
      ),
      card_body(DT::DTOutput(ns("table")))
    ),
    card(
      card_header(h5(class = "mb-0", "Project Timeline")),
      card_body(plotOutput(ns("gantt"), height = "auto"))
    )
  )
}

mod_overview_server <- function(id, data_version) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    bump <- function() data_version(data_version() + 1)
    editing_id <- reactiveVal(NULL)

    projects <- reactive({
      data_version()
      repo_projects()
    })

    filter_eq <- function(df, col, val) {
      if (is.null(val) || val == "All") return(df)
      df[!is.na(df[[col]]) & df[[col]] == val, , drop = FALSE]
    }

    filtered <- reactive({
      p <- projects()
      p <- filter_eq(p, "status", input$status)
      p <- filter_eq(p, "type_name", input$type)
      p <- filter_eq(p, "pm_name", input$pm)
      filter_eq(p, "pd_name", input$pd)
    })

    # ---- status cards (counts ignore filters; click to filter) ----
    output$cards <- renderUI({
      p <- projects()
      div(
        class = "status-cards",
        lapply(PLANNING_STATUSES, function(s) {
          actionButton(
            ns(paste0("card_", s)),
            label = tagList(span(class = "count", sum(p$status == s)), span(class = "lbl", s)),
            class = paste("status-card", paste0("card-", tolower(s)))
          )
        })
      )
    })
    lapply(PLANNING_STATUSES, function(s) {
      observeEvent(input[[paste0("card_", s)]], {
        updateSelectInput(session, "status", selected = if (identical(input$status, s)) "All" else s)
      })
    })

    # ---- cascading filter choices: each dropdown only offers valid options ----
    set_choices <- function(input_id, values) {
      cur <- isolate(input[[input_id]])
      ch <- c("All", sort(unique(values[!is.na(values)])))
      updateSelectInput(session, input_id, choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else "All")
    }
    observe({
      p <- filter_eq(projects(), "status", input$status)
      set_choices("type", p$type_name)
    })
    observe({
      p <- filter_eq(filter_eq(projects(), "status", input$status), "type_name", input$type)
      set_choices("pm", p$pm_name)
    })
    observe({
      p <- filter_eq(filter_eq(filter_eq(projects(), "status", input$status), "type_name", input$type), "pm_name", input$pm)
      set_choices("pd", p$pd_name)
    })
    observeEvent(input$reset, {
      for (i in c("status", "type", "pm", "pd")) updateSelectInput(session, i, selected = "All")
    })

    # ---- grid ----
    output$table <- DT::renderDT({
      p <- filtered()
      df <- data.frame(
        p$id, p$code %or% rep("", nrow(p)), p$name, p$type_name, p$status, p$pm_name, p$pd_name,
        as.Date(p$start_date), as.Date(p$end_date),
        stringsAsFactors = FALSE
      )
      names(df) <- c("#", "ID", "Project title", "Type", "Status", "PM", "PD", "Start date", "End date")
      df$ID[is.na(df$ID)] <- ""
      dt <- DT::datatable(
        df, rownames = FALSE, selection = "single",
        options = list(pageLength = 5, lengthMenu = c(5, 10, 25, 50), dom = "lrtip")
      )
      dt <- DT::formatDate(
        dt, c("Start date", "End date"), method = "toLocaleDateString",
        params = list("en-GB", list(day = "2-digit", month = "short", year = "numeric", timeZone = "UTC"))
      )
      DT::formatStyle(
        dt, "Status",
        backgroundColor = DT::styleEqual(
          c("Ongoing", "Incoming", "Completed", "Removed"),
          c("#d1fae5", "#dbeafe", "#f3f4f6", "#fee2e2")
        )
      )
    })

    selected_project <- reactive({
      idx <- input$table_rows_selected
      if (length(idx) != 1) return(NULL)
      filtered()[idx, , drop = FALSE]
    })

    # ---- add / edit ----
    project_modal <- function(title, p = NULL) {
      types <- repo_types()
      emps <- repo_employees()
      people <- id_choices(emps, none = "— None —")
      modalDialog(
        title = title, easyClose = TRUE,
        textInput(ns("f_code"), "ID (letters and numbers only, optional)", value = p$code %or% ""),
        textInput(ns("f_name"), "Project title", value = p$name %or% ""),
        selectInput(ns("f_type"), "Project type", choices = id_choices(types, none = "— None —"),
                    selected = as.character(p$project_type_id %or% "")),
        selectInput(ns("f_status"), "Status", choices = STATUSES, selected = p$status %or% "Incoming"),
        selectInput(ns("f_pm"), "PM", choices = people, selected = as.character(p$pm_id %or% "")),
        selectInput(ns("f_pd"), "PD", choices = people, selected = as.character(p$pd_id %or% "")),
        dateInput(ns("f_start"), "Start date", value = p$start_date %or% as.character(Sys.Date()),
                  format = "dd/mm/yyyy", weekstart = 1),
        dateInput(ns("f_end"), "End date", value = p$end_date %or% as.character(Sys.Date() + 90),
                  format = "dd/mm/yyyy", weekstart = 1),
        footer = tagList(modalButton("Cancel"), actionButton(ns("save"), "Save", class = "btn-primary"))
      )
    }

    observeEvent(input$add, {
      editing_id(NULL)
      showModal(project_modal("Add project"))
    })
    observeEvent(input$edit, {
      p <- selected_project()
      if (is.null(p)) return(showNotification("Select a project row first.", type = "warning"))
      editing_id(p$id)
      showModal(project_modal("Edit project", p))
    })

    observeEvent(input$save, {
      code <- trimws(input$f_code %or% "")
      name <- trimws(input$f_name %or% "")
      start <- input$f_start
      end <- input$f_end
      if (!nzchar(name)) return(showNotification("Project title is required.", type = "error"))
      if (nzchar(code) && !grepl("^[A-Za-z0-9]+$", code)) {
        return(showNotification("Project ID can only contain letters and numbers.", type = "error"))
      }
      if (length(start) != 1 || length(end) != 1 || is.na(start) || is.na(end) || end < start) {
        return(showNotification("Enter valid dates (end on or after start).", type = "error"))
      }
      vals <- list(
        code = if (nzchar(code)) code else NA_character_,
        name = name,
        project_type_id = as_id(input$f_type),
        status = input$f_status,
        start_date = format(start), end_date = format(end),
        pm_id = as_id(input$f_pm), pd_id = as_id(input$f_pd)
      )
      ok <- safe_run(if (is.null(editing_id())) repo_project_add(vals) else repo_project_update(editing_id(), vals))
      if (ok) {
        removeModal()
        bump()
      }
    })

    observeEvent(input$delete, {
      p <- selected_project()
      if (is.null(p)) return(showNotification("Select a project row first.", type = "warning"))
      showModal(modalDialog(
        title = "Delete project", easyClose = TRUE,
        paste0("Are you sure you want to delete “", p$name, "”?"),
        footer = tagList(modalButton("Cancel"), actionButton(ns("del_ok"), "Delete", class = "btn-danger"))
      ))
    })
    observeEvent(input$del_ok, {
      p <- selected_project()
      if (!is.null(p)) {
        safe_run(repo_project_delete(p$id))
        removeModal()
        bump()
      }
    })

    # ---- Gantt chart ----
    output$gantt <- renderPlot({
      p <- filtered()
      if (nrow(p) == 0) {
        plot.new()
        text(0.5, 0.5, "No projects match the filters.", cex = 1.2, col = "#6b7280")
        return()
      }
      p <- p[order(p$start_date, p$id), , drop = FALSE]
      p$label <- paste0(ifelse(is.na(p$code) | p$code == "", "", paste0(p$code, " — ")), p$name)
      p$y <- rev(seq_len(nrow(p)))

      par(mar = c(4, 16, 2, 2), bg = "#ffffff")
      plot(
        NA, xlim = range(c(as.Date(p$start_date), as.Date(p$end_date))),
        ylim = c(0.5, nrow(p) + 0.5), yaxt = "n", xaxt = "n",
        xlab = "", ylab = ""
      )
      axis(2, at = p$y, labels = p$label, las = 1, cex.axis = 0.85, col.axis = "#022020", tck = 0)
      axis.Date(1, at = seq(min(as.Date(p$start_date)), max(as.Date(p$end_date)), by = "month"), format = "%b %y", cex.axis = 0.85)
      grid(nx = NA, ny = NULL, col = "#e5e7eb")

      for (i in seq_len(nrow(p))) {
        col <- STATUS_COLOURS[[p$status[i]]] %or% "#007d69"
        rect(as.Date(p$start_date[i]), p$y[i] - 0.25, as.Date(p$end_date[i]), p$y[i] + 0.25, col = col, border = NA, lwd = 1)
      }
    }, height = function() max(250, nrow(filtered()) * 30 + 60))
  })
}
