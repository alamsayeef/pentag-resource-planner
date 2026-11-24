# Small shared helpers: constants, formatting, and raw-HTML inputs.
# Nothing here touches the database.

STATUSES <- c("Ongoing", "Incoming", "Completed", "Removed")
PLANNING_STATUSES <- c("Ongoing", "Incoming", "Completed")
STATUS_COLOURS <- c(Ongoing = "#00a87e", Incoming = "#60a5fa", Completed = "#9ca3af", Removed = "#fca5a5")

# Returns b when a is NULL, empty or NA
`%or%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# "" / NULL / NA -> NA_integer_, otherwise integer
as_id <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || identical(as.character(x), "")) return(NA_integer_)
  as.integer(x)
}

# Named character vector for selectInput(): names = labels, values = ids
id_choices <- function(df, label = "name", none = NULL) {
  ch <- stats::setNames(as.character(df$id), df[[label]])
  if (!is.null(none)) ch <- c(stats::setNames("", none), ch)
  ch
}

# Run a database action; show a friendly error instead of crashing the app
safe_run <- function(expr) {
  tryCatch({
    force(expr)
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("UNIQUE", msg, fixed = TRUE)) msg <- "That name, email or ID is already in use."
    shiny::showNotification(msg, type = "error", duration = 8)
    FALSE
  })
}

# ---- raw HTML inputs that report changes straight to a module's input ------
# They send list(<payload>, value, nonce) to input_id, e.g. input$emp_edit.
# This lets a table of many inputs live in one renderUI with one handler.

js_send <- function(input_id, payload) {
  sprintf(
    "Shiny.setInputValue('%s', {%s, value: this.value, nonce: Math.random()}, {priority: 'event'})",
    input_id, payload
  )
}

inline_text <- function(input_id, payload, value, width = NULL) {
  htmltools::tags$input(
    type = "text", class = "form-control form-control-sm", value = value %or% "",
    style = if (!is.null(width)) paste0("width:", width) else NULL,
    onchange = js_send(input_id, payload)
  )
}

inline_number <- function(input_id, payload, value, min = 0, max = NULL, step = 1) {
  htmltools::tags$input(
    type = "number", class = "form-control form-control-sm num-input",
    value = value, min = min, max = max, step = step,
    onchange = js_send(input_id, payload)
  )
}

inline_select <- function(input_id, payload, choices, selected = "", placeholder = "Select\u2026") {
  vals <- unname(choices)
  labs <- names(choices)
  opts <- lapply(seq_along(vals), function(i) {
    htmltools::tags$option(
      value = vals[i],
      selected = if (identical(as.character(vals[i]), as.character(selected))) NA else NULL,
      labs[i]
    )
  })
  htmltools::tags$select(
    class = "form-select form-select-sm inline-select",
    onchange = js_send(input_id, payload),
    if (!is.null(placeholder)) {
      htmltools::tags$option(value = "", selected = if (identical(as.character(selected), "")) NA else NULL, placeholder)
    },
    opts
  )
}

remove_link <- function(input_id, payload, title = "Remove") {
  htmltools::tags$a(
    href = "#", class = "remove-link", title = title,
    onclick = sprintf(
      "Shiny.setInputValue('%s', {%s, nonce: Math.random()}, {priority: 'event'}); return false;",
      input_id, payload
    ),
    htmltools::HTML("&times;")
  )
}

fmt_date <- function(x) {
  d <- as.Date(x)
  ifelse(is.na(d), "", format(d, "%d %b %Y"))
}
