# Input tab: employees, personal holidays, roles, hours per project type, and bulk Excel import/export template system.

mod_input_ui <- function(id) {
  ns <- NS(id)

  # Helper for tooltip-enabled header titles
  card_hdr_tooltip <- function(title_text, tooltip_text, btn_id, btn_label) {
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center gap-3",
        div(
          class = "d-flex align-items-center gap-2",
          h5(class = "mb-0", title_text),
          tags$span(
            class = "badge rounded-circle bg-secondary text-white",
            style = "cursor: help; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 0.75rem;",
            title = tooltip_text,
            "?"
          )
        ),
        actionButton(btn_id, btn_label, class = "btn-primary btn-sm btn-single-line")
      )
    )
  }

  tagList(
    card(
      class = "mb-3",
      card_hdr_tooltip(
        "Employees",
        "Changes save automatically. Renaming updates every tab. Inactive employees disappear from dropdowns but keep their history. Project role assignments are made on the Project tab.",
        ns("add_emp"), "+ Add employee"
      ),
      uiOutput(ns("employees"))
    ),
    card(
      class = "mb-3",
      card_header(
        div(
          class = "d-flex align-items-center gap-2",
          h5(class = "mb-0", "Personal holidays"),
          tags$span(
            class = "badge rounded-circle bg-secondary text-white",
            style = "cursor: help; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 0.75rem;",
            title = "Pick an employee, then add date ranges. Weekends inside a range are skipped automatically.",
            "?"
          )
        )
      ),
      card_body(
        selectInput(ns("leave_emp"), "Employee", choices = c("Select employee…" = ""), width = "260px"),
        uiOutput(ns("leave_ui"))
      )
    ),
    card(
      class = "mb-3",
      card_hdr_tooltip(
        "Roles",
        "Rename roles by editing them in place. PM and PD are held by each project's PM and PD and can't be removed. A new role gets 50 hours in every project type and shows as unassigned on each project.",
        ns("add_role"), "+ Add role"
      ),
      uiOutput(ns("roles"))
    ),
    card(
      class = "mb-3",
      card_hdr_tooltip(
        "Project Types — Allocated Hours",
        "Hours per role for each project type. Role hours are split equally between everyone holding the role on a project. PM and PD hours go in full to the project's PM and PD.",
        ns("add_type"), "+ Add project type"
      ),
      uiOutput(ns("types"))
    ),
    card(
      class = "mb-3",
      card_header(
        div(
          class = "d-flex align-items-center gap-2",
          h5(class = "mb-0", "Bulk Excel Import & Template"),
          tags$span(
            class = "badge rounded-circle bg-secondary text-white",
            style = "cursor: help; width: 20px; height: 20px; display: inline-flex; align-items: center; justify-content: center; font-size: 0.75rem;",
            title = "Download the structured Excel template, fill in Employees, Roles, Project Type, Allocated Hours, and Projects, then upload it to batch-update Supabase.",
            "?"
          )
        )
      ),
      card_body(
        div(
          class = "d-flex flex-wrap align-items-center gap-3",
          downloadButton(ns("dl_template"), "Download Excel Template", class = "btn-outline-primary btn-sm"),
          fileInput(ns("up_excel"), "Upload Completed Excel File (.xlsx)", accept = c(".xlsx"), buttonLabel = "Browse...", placeholder = "No file selected", width = "320px")
        )
      )
    )
  )
}

mod_input_server <- function(id, data_version) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      touch(TRUE)
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
                                    selected = as.character(e$primary_role_id[i] %or% ""), placeholder = "— None —")),
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
        selectInput(ns("n_role"), "Primary role", choices = id_choices(roles(), none = "— None —")),
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
      ch <- c("Select employee…" = "", id_choices(e))
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
            span(class = "chip", paste(fmt_date(lv_df$start_date[i]), "–", fmt_date(lv_df$end_date[i])),
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
      touch(TRUE)
    })
    pending_role <- reactiveVal(NULL)
    observeEvent(input$role_remove, {
      pending_role(as.integer(input$role_remove$id))
      r <- roles()
      nm <- r$name[r$id == pending_role()]
      showModal(modalDialog(
        title = "Remove role", easyClose = TRUE,
        paste0("Remove “", nm, "”? It disappears from all tabs and from anyone's primary role. Past data is kept."),
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

    # ================= Bulk Excel Template & Upload =================
    output$dl_template <- downloadHandler(
      filename = function() {
        paste0("pentag-resource-planner-template-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- openxlsx::createWorkbook()

        # 1. Employees sheet
        openxlsx::addWorksheet(wb, "Employees")
        emp_df <- data.frame(
          Name = c("Jane Doe", "John Smith"),
          Email = c("jane.doe@example.ac.uk", "john.smith@example.ac.uk"),
          Primary_Role = c("Role 1", "Role 2"),
          Status = c("Active", "Active"),
          FTE = c(1.0, 0.8),
          stringsAsFactors = FALSE
        )
        openxlsx::writeData(wb, "Employees", emp_df)

        # 2. Roles sheet
        openxlsx::addWorksheet(wb, "Roles")
        role_df <- data.frame(
          Role_Name = c("PM", "PD", "Role 1", "Role 2", "Role 3", "Role 4"),
          stringsAsFactors = FALSE
        )
        openxlsx::writeData(wb, "Roles", role_df)

        # 3. Project Type sheet
        openxlsx::addWorksheet(wb, "Project Type")
        pt_df <- data.frame(
          Project_Type_Name = c("Project Type 1", "Project Type 2", "Project Type 3"),
          stringsAsFactors = FALSE
        )
        openxlsx::writeData(wb, "Project Type", pt_df)

        # 4. Allocated Hours sheet
        openxlsx::addWorksheet(wb, "Allocated Hours")
        ah_df <- data.frame(
          Project_Type = c("Project Type 1", "Project Type 1", "Project Type 2"),
          Role_Name = c("Role 1", "Role 2", "Role 1"),
          Hours = c(50, 60, 45),
          stringsAsFactors = FALSE
        )
        openxlsx::writeData(wb, "Allocated Hours", ah_df)

        # 5. Projects sheet (covering remainder of 2026 and all of 2027)
        openxlsx::addWorksheet(wb, "Projects")
        proj_df <- data.frame(
          Project_Code = c("P202-1", "P202-2"),
          Title = c("Health Economics Evaluation 2026", "Clinical Pathway Model 2027"),
          Project_Type = c("Project Type 1", "Project Type 2"),
          Status = c("Ongoing", "Incoming"),
          Start_Date = c("2026-09-01", "2027-01-15"),
          End_Date = c("2026-12-31", "2027-12-31"),
          PM_Email = c("jane.doe@example.ac.uk", "john.smith@example.ac.uk"),
          PD_Email = c("john.smith@example.ac.uk", "jane.doe@example.ac.uk"),
          stringsAsFactors = FALSE
        )
        openxlsx::writeData(wb, "Projects", proj_df)

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    observeEvent(input$up_excel, {
      req(input$up_excel)
      inFile <- input$up_excel
      if (is.null(inFile)) return()

      tryCatch({
        path <- inFile$datapath
        sheets <- readxl::excel_sheets(path)

        db_tx(function(con) {
          # 1. Import Roles
          if ("Roles" %in% sheets) {
            rdf <- readxl::read_excel(path, sheet = "Roles")
            for (i in seq_len(nrow(rdf))) {
              rname <- trimws(as.character(rdf$Role_Name[i]))
              if (nzchar(rname)) {
                ex <- DBI::dbGetQuery(con, "SELECT id FROM roles WHERE name = ?", params = list(rname))
                if (nrow(ex) == 0) {
                  DBI::dbExecute(con, "INSERT INTO roles (name) VALUES (?)", params = list(rname))
                }
              }
            }
          }

          # 2. Import Project Types
          if ("Project Type" %in% sheets) {
            pdf <- readxl::read_excel(path, sheet = "Project Type")
            for (i in seq_len(nrow(pdf))) {
              ptname <- trimws(as.character(pdf$Project_Type_Name[i]))
              if (nzchar(ptname)) {
                ex <- DBI::dbGetQuery(con, "SELECT id FROM project_types WHERE name = ?", params = list(ptname))
                if (nrow(ex) == 0) {
                  DBI::dbExecute(con, "INSERT INTO project_types (name) VALUES (?)", params = list(ptname))
                }
              }
            }
          }

          # 3. Import Employees
          if ("Employees" %in% sheets) {
            edf <- readxl::read_excel(path, sheet = "Employees")
            for (i in seq_len(nrow(edf))) {
              ename <- trimws(as.character(edf$Name[i]))
              if (!nzchar(ename)) next
              eemail <- trimws(as.character(edf$Email[i] %or% NA))
              if (identical(eemail, "NA") || !nzchar(eemail)) eemail <- NA_character_
              efte <- suppressWarnings(as.numeric(edf$FTE[i]))
              if (is.na(efte)) efte <- 1.0
              active_val <- if (!is.null(edf$Status) && tolower(as.character(edf$Status[i])) == "inactive") 0 else 1

              rname <- trimws(as.character(edf$Primary_Role[i] %or% ""))
              rid <- NA_integer_
              if (nzchar(rname) && rname != "NA") {
                rex <- DBI::dbGetQuery(con, "SELECT id FROM roles WHERE name = ?", params = list(rname))
                if (nrow(rex) > 0) rid <- rex$id[1]
              }

              emex <- DBI::dbGetQuery(con, "SELECT id FROM employees WHERE name = ? OR (email IS NOT NULL AND email = ?)", params = list(ename, eemail))
              if (nrow(emex) > 0) {
                DBI::dbExecute(con, "UPDATE employees SET email = ?, fte = ?, is_active = ?, primary_role_id = ? WHERE id = ?",
                               params = list(eemail, efte, active_val, rid, emex$id[1]))
              } else {
                DBI::dbExecute(con, "INSERT INTO employees (name, email, fte, is_active, primary_role_id) VALUES (?, ?, ?, ?, ?)",
                               params = list(ename, eemail, efte, active_val, rid))
              }
            }
          }

          # 4. Import Allocated Hours
          if ("Allocated Hours" %in% sheets) {
            ahdf <- readxl::read_excel(path, sheet = "Allocated Hours")
            for (i in seq_len(nrow(ahdf))) {
              ptname <- trimws(as.character(ahdf$Project_Type[i]))
              rname <- trimws(as.character(ahdf$Role_Name[i]))
              hrs <- suppressWarnings(as.numeric(ahdf$Hours[i]))
              if (!nzchar(ptname) || !nzchar(rname) || is.na(hrs)) next

              ptid <- DBI::dbGetQuery(con, "SELECT id FROM project_types WHERE name = ?", params = list(ptname))$id
              rid <- DBI::dbGetQuery(con, "SELECT id FROM roles WHERE name = ?", params = list(rname))$id
              if (length(ptid) == 1 && length(rid) == 1) {
                DBI::dbExecute(con, paste(
                  "INSERT INTO role_hours (project_type_id, role_id, hours) VALUES (?, ?, ?)",
                  "ON CONFLICT(project_type_id, role_id) DO UPDATE SET hours = excluded.hours"
                ), params = list(ptid, rid, hrs))
              }
            }
          }

          # 5. Import Projects
          if ("Projects" %in% sheets) {
            prjdf <- readxl::read_excel(path, sheet = "Projects")
            for (i in seq_len(nrow(prjdf))) {
              pcode <- trimws(as.character(prjdf$Project_Code[i] %or% ""))
              if (identical(pcode, "NA") || !nzchar(pcode)) pcode <- NULL
              pname <- trimws(as.character(prjdf$Title[i]))
              if (!nzchar(pname)) next
              ptname <- trimws(as.character(prjdf$Project_Type[i] %or% ""))
              pstatus <- trimws(as.character(prjdf$Status[i] %or% "Incoming"))
              sdate <- as.character(prjdf$Start_Date[i])
              edate <- as.character(prjdf$End_Date[i])

              pm_email <- trimws(as.character(prjdf$PM_Email[i] %or% ""))
              pd_email <- trimws(as.character(prjdf$PD_Email[i] %or% ""))

              ptid <- if (nzchar(ptname)) DBI::dbGetQuery(con, "SELECT id FROM project_types WHERE name = ?", params = list(ptname))$id else NA_integer_
              if (length(ptid) == 0) ptid <- NA_integer_

              pm_id <- if (nzchar(pm_email)) DBI::dbGetQuery(con, "SELECT id FROM employees WHERE email = ?", params = list(pm_email))$id else NA_integer_
              if (length(pm_id) == 0) pm_id <- NA_integer_

              pd_id <- if (nzchar(pd_email)) DBI::dbGetQuery(con, "SELECT id FROM employees WHERE email = ?", params = list(pd_email))$id else NA_integer_
              if (length(pd_id) == 0) pd_id <- NA_integer_

              pex <- if (!is.null(pcode)) DBI::dbGetQuery(con, "SELECT id FROM projects WHERE code = ?", params = list(pcode)) else data.frame(id = integer())
              if (nrow(pex) > 0) {
                DBI::dbExecute(con, "UPDATE projects SET name = ?, project_type_id = ?, status = ?, start_date = ?, end_date = ?, pm_id = ?, pd_id = ? WHERE id = ?",
                               params = list(pname, ptid, pstatus, sdate, edate, pm_id, pd_id, pex$id[1]))
              } else {
                DBI::dbExecute(con, "INSERT INTO projects (code, name, project_type_id, status, start_date, end_date, pm_id, pd_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                               params = list(pcode, pname, ptid, pstatus, sdate, edate, pm_id, pd_id))
              }
            }
          }
        })

        showNotification("Excel file successfully imported and synced with Supabase!", type = "message")
        touch(TRUE)
      }, error = function(e) {
        showNotification(paste("Import failed:", e$message), type = "error", duration = 10)
      })
    })
  })
}
