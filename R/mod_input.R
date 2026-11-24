# Input tab: employees, personal holidays, roles, and hours per project type.
# Cells save as soon as you leave them. Edits that don't change the layout
# (names, FTE, hours...) do not re-draw this tab, so keyboard focus is kept.

mod_input_ui <- function(id) {
  ns <- NS(id)
  hdr <- function(title, note, btn_id, btn_label) {
    card_header(
      div(
        class = "d-flex justify-content-between align-items-start gap-3",
        div(h5(class = "mb-1", title), p(class = "text-muted small mb-0", note)),
        actionButton(btn_id, btn_label, class = "btn-primary btn-sm")
      )
    )
  }
  tagList(
    card(
      class = "mb-3",
      hdr("Employees",
          "Changes save automatically. Renaming updates every tab. Inactive employees disappear from dropdowns but keep their history. Project role assignments are made on the Project tab.",
          ns("add_emp"), "+ Add employee"),
      uiOutput(ns("employees"))
    ),
    card(
      class = "mb-3",
      card_header(h5(class = "mb-1", "Personal holidays"),
                  p(class = "text-muted small mb-0", "Pick an employee, then add date ranges. Weekends inside a range are skipped automatically.")),
      card_body(
        selectInput(ns("leave_emp"), "Employee", choices = c("Select employee\u2026" = ""), width = "260px"),
        uiOutput(ns("leave_ui"))
      )
    ),
    card(
      class = "mb-3",
      hdr("Roles",
          "Rename roles by editing them in place. PM and PD are held by each project's PM and PD and can't be removed. A new role gets 50 hours in every project type and shows as unassigned on each project.",
          ns("add_role"), "+ Add role"),
      uiOutput(ns("roles"))
    ),
    card(
      hdr("Project Types \u2014 Allocated Hours",
          "Hours per role for each project type. Role hours are split equally between everyone holding the role on a project. PM and PD hours go in full to the project's PM and PD.",
          ns("add_type"), "+ Add project type"),
      uiOutput(ns("types"))
    )
  )
}

mod_input_server <- function(id, data_version) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # lv re-draws this tab; data_version tells the other tabs to re-read.
    lv <- reactiveVal(0)
    touch <- function(redraw = FALSE) {
      data_version(data_version() + 1)
      if (redraw) lv(lv() + 1)
    }

    employees <- reactive({ lv(); repo_employees(all = TRUE) })
    roles <- reactive({ lv(); repo_roles() })
    types <- reactive({ lv(); repo_types() })

    invalid <- function(msg = "Invalid value.") {
      showNotification(msg, type = "error")
      touch(TRUE) # redraw to restore the stored value
    }

    # ================= Employees =================
    output$employees <- renderUI({
      e <- employees()
      rc <- id_choices(roles())
      div(
        class = "table-responsive",
        tags$table(
          class = "table table-sm align-middle",
          tags$thead(tags$tr(lapply(c("Employee", "Email", "Primary role", "Employment status", "FTE"), tags$th))),
          tags$tbody(lapply(seq_len(nrow(e)), function(i) {
            pl <- function(field) sprintf("id: %d, field: '%s'", e$id[i], field)
            tags$tr(
              tags$td(inline_text(ns("emp_edit"), pl("name"), e$name[i], width = "200px")),
              tags$td(inline_text(ns("emp_edit"), pl("email"), e$email[i], width = "220px")),
              tags$td(inline_select(ns("emp_edit"), pl("primary_role_id"), rc,
                                    selected = as.character(e$primary_role_id[i] %or% ""), placeholder = "\u2014 None \u2014")),
              tags$td(inline_select(ns("emp_edit"), pl("is_active"), c(Active = "1", Inactive = "0"),
                                    selected = as.character(e$is_active[i]), placeholder = NULL)),
              tags$td(inline_number(ns("emp_edit"), pl("fte"), e$fte[i], min = 0, max = 1, step = 0.1))
            )
          }))
        )
      )
    })

    observeEvent(input$emp_edit, {
      a <- input$emp_edit
      id <- as.integer(a$id)
      v <- a$value
      vals <- switch(
        a$field,
        name = if (nzchar(trimws(v))) list(name = trimws(v)) else NULL,
        email = list(email = if (nzchar(trimws(v))) trimws(v) else NA_character_),
        primary_role_id = list(primary_role_id = as_id(v)),
        is_active = list(is_active = as.integer(v)),
        fte = {
          n <- suppressWarnings(as.numeric(v))
          if (is.na(n) || n < 0 || n > 1) NULL else list(fte = n)
        },
        NULL
      )
      if (is.null(vals)) return(invalid("Enter a valid value (name can't be empty, FTE between 0 and 1)."))
      if (safe_run(repo_employee_update(id, vals))) touch(FALSE) else touch(TRUE)
    })

    observeEvent(input$add_emp, {
      showModal(modalDialog(
        title = "Add employee", easyClose = TRUE,
        textInput(ns("n_name"), "Name"),
        textInput(ns("n_email"), "Email (optional)"),
        selectInput(ns("n_role"), "Primary role", choices = id_choices(roles(), none = "\u2014 None \u2014")),
        numericInput(ns("n_fte"), "FTE", value = 1, min = 0, max = 1, step = 0.1),
        footer = tagList(modalButton("Cancel"), actionButton(ns("save_emp"), "Save employee", class = "btn-primary"))
      ))
    })
    observeEvent(input$save_emp, {
      name <- trimws(input$n_name %or% "")
      fte <- input$n_fte
      if (!nzchar(name) || length(fte) != 1 || is.na(fte) || fte < 0 || fte > 1) {
        return(showNotification("Enter a name and an FTE between 0 and 1.", type = "error"))
      }
      email <- trimws(input$n_email %or% "")
      ok <- safe_run(repo_employee_add(name, if (nzchar(email)) email else NA_character_, fte, as_id(input$n_role)))
      if (ok) {
        removeModal()
        touch(TRUE)
      }
    })

    # ================= Personal holidays =================
    observe({
      data_version()
      lv()
      e <- repo_employees(all = TRUE)
      cur <- isolate(input$leave_emp)
      ch <- c("Select employee\u2026" = "", id_choices(e))
      updateSelectInput(session, "leave_emp", choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else "")
    })
    leave_v <- reactiveVal(0)
    output$leave_ui <- renderUI({
      leave_v()
      if (is.null(input$leave_emp) || input$leave_emp == "") return(p(class = "text-muted", "Select an employee to see and add holidays."))
      lv_df <- repo_leave(as.integer(input$leave_emp))
      tagList(
        div(
          class = "chips",
          if (nrow(lv_df) == 0) span(class = "text-muted small", "No personal holidays"),
          lapply(seq_len(nrow(lv_df)), function(i) {
            span(class = "chip", paste(fmt_date(lv_df$start_date[i]), "\u2013", fmt_date(lv_df$end_date[i])),
                 remove_link(ns("leave_del"), sprintf("id: %d", lv_df$id[i]), "Remove holiday"))
          })
        ),
        div(
          class = "d-flex align-items-end gap-2 mt-3",
          dateRangeInput(ns("leave_range"), "Add holiday", start = Sys.Date(), end = Sys.Date(),
                         format = "dd/mm/yyyy", weekstart = 1),
          actionButton(ns("leave_add"), "Add", class = "btn-primary btn-sm mb-3")
        )
      )
    })
    observeEvent(input$leave_add, {
      r <- input$leave_range
      if (is.null(input$leave_emp) || input$leave_emp == "" || length(r) != 2 || any(is.na(r)) || r[2] < r[1]) {
        return(showNotification("Choose an employee and a valid date range.", type = "error"))
      }
      repo_leave_add(as.integer(input$leave_emp), r[1], r[2])
      leave_v(leave_v() + 1)
      touch(FALSE)
    })
    observeEvent(input$leave_del, {
      repo_leave_delete(as.integer(input$leave_del$id))
      leave_v(leave_v() + 1)
      touch(FALSE)
    })
    observeEvent(input$leave_emp, leave_v(leave_v() + 1), ignoreInit = TRUE)

    # ================= Roles =================
    output$roles <- renderUI({
      r <- roles()
      div(
        class = "chips p-3",
        lapply(seq_len(nrow(r)), function(i) {
          div(
            class = "d-flex align-items-center gap-1",
            inline_text(ns("role_rename"), sprintf("id: %d", r$id[i]), r$name[i], width = "190px"),
            if (is.na(r$system_key[i])) remove_link(ns("role_remove"), sprintf("id: %d", r$id[i]), "Remove role")
          )
        })
      )
    })
    observeEvent(input$role_rename, {
      a <- input$role_rename
      v <- trimws(a$value)
      if (!nzchar(v)) return(invalid("Role name can't be empty."))
      safe_run(repo_role_rename(as.integer(a$id), v))
      touch(TRUE) # headers in the hours table show role names
    })
    pending_role <- reactiveVal(NULL)
    observeEvent(input$role_remove, {
      pending_role(as.integer(input$role_remove$id))
      r <- roles()
      nm <- r$name[r$id == pending_role()]
      showModal(modalDialog(
        title = "Remove role", easyClose = TRUE,
        paste0("Remove \u201c", nm, "\u201d? It disappears from all tabs and from anyone's primary role. Past data is kept."),
        footer = tagList(modalButton("Cancel"), actionButton(ns("role_remove_ok"), "Remove", class = "btn-danger"))
      ))
    })
    observeEvent(input$role_remove_ok, {
      if (safe_run(repo_role_remove(pending_role()))) removeModal()
      touch(TRUE)
    })
    observeEvent(input$add_role, {
      showModal(modalDialog(
        title = "Add role", easyClose = TRUE,
        textInput(ns("new_role"), "Role name"),
        footer = tagList(modalButton("Cancel"), actionButton(ns("save_role"), "Save role", class = "btn-primary"))
      ))
    })
    observeEvent(input$save_role, {
      n <- trimws(input$new_role %or% "")
      if (!nzchar(n)) return(showNotification("Enter a role name.", type = "error"))
      if (safe_run(repo_role_add(n))) {
        removeModal()
        touch(TRUE)
      }
    })

    # ================= Project types x role hours =================
    output$types <- renderUI({
      r <- roles()
      t <- types()
      rh <- repo_role_hours()
      div(
        class = "table-responsive",
        tags$table(
          class = "table table-sm align-middle",
          tags$thead(tags$tr(tags$th("Project type"), lapply(r$name, tags$th))),
          tags$tbody(lapply(seq_len(nrow(t)), function(i) {
            tags$tr(
              tags$td(inline_text(ns("type_rename"), sprintf("id: %d", t$id[i]), t$name[i], width = "200px")),
              lapply(seq_len(nrow(r)), function(j) {
                h <- rh$hours[rh$project_type_id == t$id[i] & rh$role_id == r$id[j]]
                tags$td(inline_number(ns("type_hours"), sprintf("type: %d, role: %d", t$id[i], r$id[j]),
                                      if (length(h) == 1) h else 0, min = 0, step = 1))
              })
            )
          }))
        )
      )
    })
    observeEvent(input$type_hours, {
      a <- input$type_hours
      n <- suppressWarnings(as.numeric(a$value))
      if (is.na(n) || n < 0) return(invalid("Hours must be a number of 0 or more."))
      if (safe_run(repo_set_hours(as.integer(a$type), as.integer(a$role), n))) touch(FALSE) else touch(TRUE)
    })
    observeEvent(input$type_rename, {
      a <- input$type_rename
      v <- trimws(a$value)
      if (!nzchar(v)) return(invalid("Project type name can't be empty."))
      if (safe_run(repo_type_rename(as.integer(a$id), v))) touch(FALSE) else touch(TRUE)
    })
    observeEvent(input$add_type, {
      showModal(modalDialog(
        title = "Add project type", easyClose = TRUE,
        textInput(ns("new_type"), "Project type name"),
        p(class = "text-muted small", "New types start with 50 hours for every role."),
        footer = tagList(modalButton("Cancel"), actionButton(ns("save_type"), "Save project type", class = "btn-primary"))
      ))
    })
    observeEvent(input$save_type, {
      n <- trimws(input$new_type %or% "")
      if (!nzchar(n)) return(showNotification("Enter a name.", type = "error"))
      if (safe_run(repo_type_add(n))) {
        removeModal()
        touch(TRUE)
      }
    })
  })
}
