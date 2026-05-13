# ==============================================================================
# Indicator: Approved vs Unpaid Claims
# Spec: requirements/approved_unpaid_claims_summary.md
# ==============================================================================

# ---- Data --------------------------------------------------------------------

.APX_FUNDS <- c("SHIF", "ECCIF", "POMSF", "PHC")

.APX_COUNTIES <- c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret",
                   "Thika", "Kitale", "Garissa", "Meru", "Nyeri")

.APX_PROVIDERS <- list(
  Nairobi = c("Kenyatta National Hospital", "Nairobi Hospital",
               "Aga Khan Hospital", "MP Shah Hospital"),
  Mombasa = c("Coast General Hospital", "Mombasa Hospital",
               "Pandya Memorial Hospital"),
  Kisumu  = c("Jaramogi Oginga Odinga Hospital",
               "Kisumu County Referral Hospital"),
  Nakuru  = c("Nakuru Level 5 Hospital", "War Memorial Hospital",
               "Rift Valley Hospital"),
  Eldoret = c("Moi Teaching & Referral Hospital", "Eldoret Hospital"),
  Thika   = c("Thika Level 5 Hospital", "Mama Lucy Kibaki Hospital"),
  Kitale  = c("Kitale County Referral Hospital", "Trans Nzoia Hospital"),
  Garissa = c("Garissa County Referral Hospital", "Ijara Hospital"),
  Meru    = c("Meru Level 5 Hospital", "Consolata Hospital Nkubu"),
  Nyeri   = c("Nyeri County Referral Hospital", "Mount Kenya Hospital")
)

.make_apx_fund <- function(fund) {
  sb <- match(fund, .APX_FUNDS) * 77
  lapply(seq_along(.APX_COUNTIES), function(i) {
    co <- .APX_COUNTIES[[i]]
    pv <- .APX_PROVIDERS[[co]]
    n  <- length(pv)
    set.seed(sb + i * 13)
    tc <- sample(100:900, n, replace = TRUE)
    uc <- pmax(1L, round(tc * runif(n, .05, .40)))
    tv <- runif(n, 2e6, 20e6)
    uv <- tv * runif(n, .05, .40)
    list(
      county        = co,
      cid           = paste0("c-", tolower(fund), "-", i),
      total_claims  = sum(tc),
      unpaid_claims = sum(uc),
      total_value   = sum(tv),
      unpaid_value  = sum(uv),
      providers     = data.frame(
        name          = pv,
        total_claims  = tc,
        unpaid_claims = uc,
        total_value   = tv,
        unpaid_value  = uv,
        stringsAsFactors = FALSE
      )
    )
  })
}

apx_data <- setNames(lapply(.APX_FUNDS, .make_apx_fund), .APX_FUNDS)

apx_metrics <- lapply(apx_data, function(fd) list(
  total_claims  = sum(sapply(fd, `[[`, "total_claims")),
  unpaid_claims = sum(sapply(fd, `[[`, "unpaid_claims")),
  total_value   = sum(sapply(fd, `[[`, "total_value")),
  unpaid_value  = sum(sapply(fd, `[[`, "unpaid_value"))
))

apx_overall_metrics <- list(
  total_claims  = sum(sapply(apx_metrics, `[[`, "total_claims")),
  unpaid_claims = sum(sapply(apx_metrics, `[[`, "unpaid_claims")),
  total_value   = sum(sapply(apx_metrics, `[[`, "total_value")),
  unpaid_value  = sum(sapply(apx_metrics, `[[`, "unpaid_value"))
)

apx_overall_county <- lapply(seq_along(.APX_COUNTIES), function(i) {
  co <- .APX_COUNTIES[[i]]
  tc <- sum(sapply(.APX_FUNDS, function(f) apx_data[[f]][[i]]$total_claims))
  uc <- sum(sapply(.APX_FUNDS, function(f) apx_data[[f]][[i]]$unpaid_claims))
  tv <- sum(sapply(.APX_FUNDS, function(f) apx_data[[f]][[i]]$total_value))
  uv <- sum(sapply(.APX_FUNDS, function(f) apx_data[[f]][[i]]$unpaid_value))
  prov_mat <- function(col)
    do.call(cbind, lapply(.APX_FUNDS, function(f) apx_data[[f]][[i]]$providers[[col]]))
  list(
    county        = co,
    cid           = paste0("c-overall-", i),
    total_claims  = tc,
    unpaid_claims = uc,
    total_value   = tv,
    unpaid_value  = uv,
    providers     = data.frame(
      name          = .APX_PROVIDERS[[co]],
      total_claims  = rowSums(prov_mat("total_claims")),
      unpaid_claims = rowSums(prov_mat("unpaid_claims")),
      total_value   = rowSums(prov_mat("total_value")),
      unpaid_value  = rowSums(prov_mat("unpaid_value")),
      stringsAsFactors = FALSE
    )
  )
})

# ---- Component builders ------------------------------------------------------

.apx_metric_card <- function(label, value, icon_cls, color) {
  div(class = "col",
    div(class = "card border-0 shadow-sm h-100",
      div(class = "card-body d-flex align-items-center gap-3 p-4",
        div(
          class = "flex-shrink-0 rounded-3 d-flex align-items-center justify-content-center",
          style = paste0("width:52px; height:52px; background:", color, "18;"),
          tags$i(class = icon_cls, style = paste0("font-size:1.4rem; color:", color, ";"))
        ),
        div(class = "flex-grow-1 min-width-0",
          div(class = "text-uppercase fw-semibold text-muted mb-1 text-truncate",
              style = "font-size:.7rem; letter-spacing:.07em;", label),
          div(class = "fw-bold lh-sm", style = "font-size:1.5rem; color:#0f172a;", value)
        )
      )
    )
  )
}

.apx_metrics_row <- function(m) {
  div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-4 g-3",
    .apx_metric_card("Total Claims",           fmt_num(m$total_claims),
                     "bi bi-file-earmark-medical-fill", "#0284c7"),
    .apx_metric_card("Total Value",            fmt_currency(m$total_value),
                     "bi bi-cash-stack",                "#7c3aed"),
    .apx_metric_card("Unpaid Claims",          fmt_num(m$unpaid_claims),
                     "bi bi-hourglass-split",           "#d97706"),
    .apx_metric_card("Value of Unpaid Claims", fmt_currency(m$unpaid_value),
                     "bi bi-exclamation-circle-fill",   "#dc2626")
  )
}

.apx_county_table <- function(fd, fund) {
  sid  <- paste0("apx-search-", tolower(fund))
  rows <- lapply(seq_along(fd), function(i) {
    d    <- fd[[i]]
    pc <- d$unpaid_claims / d$total_claims * 100
    pv <- d$unpaid_value  / d$total_value  * 100
    county_row <- tags$tr(
      class = paste("county-row", if (i %% 2 == 0) "county-row-alt" else ""),
      `data-county-id` = d$cid,
      tags$td(class = "fw-semibold county-name-cell",
        tags$i(class = "bi bi-chevron-right expand-chevron me-2",
               style = "font-size:.8rem; color:#94a3b8;"),
        d$county),
      tags$td(fmt_num(d$total_claims)),
      tags$td(fmt_num(d$unpaid_claims)),
      tags$td(pct_badge(pc, 15, 25, higher_is_bad = TRUE)),
      tags$td(fmt_currency(d$total_value)),
      tags$td(fmt_currency(d$unpaid_value)),
      tags$td(pct_badge(pv, 15, 25, higher_is_bad = TRUE))
    )
    prov_rows <- lapply(seq_len(nrow(d$providers)), function(j) {
      p    <- d$providers[j, ]
      pc2  <- p$unpaid_claims / p$total_claims * 100
      pvc2 <- p$unpaid_value  / p$total_value  * 100
      tags$tr(
        class = paste("provider-row", d$cid), style = "display:none;",
        tags$td(class = "provider-name-cell",
          tags$i(class = "bi bi-building me-2",
                 style = "font-size:.8rem; color:#94a3b8;"),
          tags$span(class = "text-secondary", style = "font-size:.9rem;", p$name)),
        tags$td(class = "text-secondary", fmt_num(p$total_claims)),
        tags$td(class = "text-secondary", fmt_num(p$unpaid_claims)),
        tags$td(pct_badge(pc2, 15, 25, higher_is_bad = TRUE)),
        tags$td(class = "text-secondary", fmt_currency(p$total_value)),
        tags$td(class = "text-secondary", fmt_currency(p$unpaid_value)),
        tags$td(pct_badge(pvc2, 15, 25, higher_is_bad = TRUE))
      )
    })
    tagList(county_row, do.call(tagList, prov_rows))
  })

  div(class = "card border-0 shadow-sm mt-4",
    div(class = "card-header bg-white border-bottom d-flex justify-content-between align-items-center py-3 px-4",
      div(class = "fw-semibold", style = "color:#0f172a;", "Claims by County"),
      div(class = "input-group input-group-sm apx-search-wrap", style = "width:220px;",
        tags$span(class = "input-group-text border-end-0",
          tags$i(class = "bi bi-search", style = "font-size:.8rem; color:#94a3b8;")),
        tags$input(type = "text", id = sid, `data-fund` = fund,
          class = "form-control border-start-0 apx-search",
          placeholder = "Search county...")
      )
    ),
    div(class = "table-responsive",
      tags$table(
        class = "table table-hover align-middle mb-0", `data-fund` = fund,
        tags$thead(class = "table-light",
          tags$tr(
            col_th("County / Provider", "240px"),
            col_th("Total Claims"),
            col_th("Claims"),
            col_th("% Unpaid Claims"),
            col_th("Total Value"),
            col_th("Unpaid Claims Value"),
            col_th("% Value")
          )
        ),
        tags$tbody(do.call(tagList, rows))
      )
    ),
    div(class = "card-footer bg-white border-top d-flex justify-content-between align-items-center py-2 px-4",
      tags$span(class = "text-muted small",
                sprintf("Showing %d of %d counties", length(fd), length(fd))),
      make_pagination(1L, 1L, paste0("apx_page_", tolower(fund)))
    )
  )
}

.apx_tab_pane <- function(fund, data, metrics, is_active = FALSE) {
  div(id = paste0("apx-pane-", fund),
    class = paste("tab-pane fade", if (is_active) "show active" else ""),
    role = "tabpanel",
    div(class = "pt-3",
      .apx_metrics_row(metrics),
      .apx_county_table(data, fund)
    )
  )
}

# ---- Public: panel UI --------------------------------------------------------

apx_panel_ui <- function() {
  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "Unpaid Claims in ERP",
      last_updated = "13 May 2026",
      source = "Payer System",
      info = "Claims that have been approved and are payment-ready but have not yet been disbursed in the ERP system. Broken down by fund (SHIF, ECCIF, POMSF) and drillable to county and provider level.",
      badges = tagList(
        tags$span(class = "badge text-bg-primary px-3 py-2 rounded-pill",   "sha_user"),
        tags$span(class = "badge text-bg-secondary px-3 py-2 rounded-pill", "dha_user")
      )
    ),

    # Fund-type tabs (Overall / SHIF / ECCIF / POMSF)
    tags$ul(class = "nav nav-pills gap-1 mb-1", role = "tablist",
      lapply(c("Overall", .APX_FUNDS), function(f) {
        tags$li(class = "nav-item", role = "presentation",
          tags$button(
            class = paste("nav-link", if (f == "Overall") "active" else ""),
            id = paste0("apx-tab-", f),
            `data-bs-toggle` = "pill",
            `data-bs-target` = paste0("#apx-pane-", f),
            type = "button", role = "tab",
            f
          )
        )
      })
    ),
    div(class = "tab-content",
      .apx_tab_pane("Overall", apx_overall_county, apx_overall_metrics, is_active = TRUE),
      do.call(tagList, lapply(.APX_FUNDS, function(f)
        .apx_tab_pane(f, apx_data[[f]], apx_metrics[[f]])
      ))
    )
  )
}

# ---- Public: server ----------------------------------------------------------

apx_server <- function(input, output, session) {
  # All content is pre-rendered; county expand/collapse and search are handled
  # by the shared client-side JS in app.R.
  invisible(NULL)
}
