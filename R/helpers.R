# Shared formatting and UI helpers used across all indicators.

fmt_num <- function(x) formatC(round(x), format = "d", big.mark = ",")

fmt_currency <- function(x) {
  if (x >= 1e9) paste0("KES ", round(x / 1e9, 2), "B")
  else if (x >= 1e6) paste0("KES ", round(x / 1e6, 1), "M")
  else paste0("KES ", fmt_num(x))
}

# Percentage badge: colour depends on whether higher is bad or good.
pct_badge <- function(x, lo, hi, higher_is_bad = TRUE) {
  if (higher_is_bad) {
    cls <- if (x > hi) "badge bg-danger"
           else if (x > lo) "badge bg-warning text-dark"
           else "badge bg-success"
  } else {
    cls <- if (x < lo) "badge bg-danger"
           else if (x < hi) "badge bg-warning text-dark"
           else "badge bg-success"
  }
  tags$span(class = cls, paste0(round(x, 1), "%"))
}

# Delta badge shown on metric cards.
delta_tag <- function(delta, lower_is_better = FALSE, suffix = "") {
  if (delta == 0) return(tags$span(class = "text-muted", style = "font-size:.75rem;", "—"))
  is_pos  <- delta > 0
  is_good <- xor(is_pos, lower_is_better)
  color   <- if (is_good) "#16a34a" else "#dc2626"
  arrow   <- if (is_pos) "bi-arrow-up-short" else "bi-arrow-down-short"
  sign    <- if (is_pos) "+" else ""
  tags$span(
    style = paste0("font-size:.75rem; font-weight:600; color:", color, ";"),
    tags$i(class = paste("bi", arrow)),
    paste0(sign, delta, suffix)
  )
}

# Sortable column header for tables.
col_th <- function(label, min_width = NULL) {
  tags$th(
    class = "sortable-col",
    style = if (!is.null(min_width)) paste0("min-width:", min_width, ";") else "",
    label,
    tags$i(class = "bi bi-chevron-expand ms-1 sort-icon",
           style = "font-size:.7rem; color:#94a3b8;")
  )
}

# Bootstrap pagination controls. Clicks send page number to `input[[input_name]]`.
make_pagination <- function(current_page, total_pages, input_name) {
  js <- function(p) sprintf(
    "Shiny.setInputValue('%s',%d,{priority:'event'});return false;", input_name, p)

  prev_cls <- if (current_page <= 1) "page-item disabled" else "page-item"
  next_cls <- if (current_page >= total_pages) "page-item disabled" else "page-item"

  tags$nav(
    tags$ul(class = "pagination pagination-sm mb-0",
      tags$li(class = prev_cls,
        tags$a(class = "page-link", href = "#",
               onclick = if (current_page > 1) js(current_page - 1) else "return false;",
               HTML("&laquo;"))),
      lapply(seq_len(total_pages), function(p) {
        tags$li(class = if (p == current_page) "page-item active" else "page-item",
          tags$a(class = "page-link", href = "#", onclick = js(p), as.character(p)))
      }),
      tags$li(class = next_cls,
        tags$a(class = "page-link", href = "#",
               onclick = if (current_page < total_pages) js(current_page + 1) else "return false;",
               HTML("&raquo;")))
    )
  )
}

# Shared page-subtitle bar used at the top of each indicator content area.
indicator_header <- function(title, subtitle, badges = NULL,
                             last_updated = NULL, source = NULL, info = NULL) {
  right_content <- tagList(
    if (!is.null(badges))
      div(class = "d-flex gap-2 flex-wrap justify-content-end mb-1", badges),
    if (!is.null(last_updated))
      div(class = "d-flex align-items-center gap-1 justify-content-end mt-1",
        style = "font-size:.72rem; color:#64748b;",
        tags$i(class = "bi bi-clock-history", style = "font-size:.72rem;"),
        tags$span(paste0("Last updated: ", last_updated))
      ),
    if (!is.null(source))
      div(class = "d-flex align-items-center gap-1 justify-content-end mt-1",
        style = "font-size:.72rem; color:#64748b;",
        tags$i(class = "bi bi-database", style = "font-size:.72rem;"),
        tags$span(paste0("Source: ", source))
      )
  )
  div(
    class = "d-flex justify-content-between align-items-start mb-4",
    div(
      tags$h5(class = "fw-bold mb-0 d-flex align-items-center gap-2",
        style = "color:#0f172a;",
        title,
        if (!is.null(info))
          tags$i(
            class = "bi bi-info-circle",
            style = "font-size:.95rem; color:#94a3b8; cursor:default;",
            `data-bs-toggle`    = "tooltip",
            `data-bs-placement` = "right",
            `data-bs-html`      = "true",
            title = info
          )
      ),
      tags$p(class = "text-muted mb-0 mt-1", style = "font-size:.84rem;",
             HTML(subtitle))
    ),
    div(class = "flex-shrink-0 text-end", right_content)
  )
}
