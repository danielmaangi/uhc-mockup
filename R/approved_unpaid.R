# ==============================================================================
# Indicator: Approved vs Unpaid Claims
# Spec: requirements/approved_unpaid_claims_summary.md
# ==============================================================================

# ---- Data --------------------------------------------------------------------

.APX_FUNDS    <- c("SHIF", "ECCIF", "POMSF", "PHC")
.APX_COUNTIES <- c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret",
                   "Thika", "Kitale", "Garissa", "Meru", "Nyeri")
.APX_LEVELS   <- c("Level 2", "Level 3", "Level 4", "Level 5", "National Referral")
.apx_months   <- format(seq(as.Date("2025-05-01"), by = "month", length.out = 12), "%b '%y")

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

# ---- Chart.js head content ---------------------------------------------------

# Per-fund monthly unpaid data (value + count)
.apx_fund_monthly <- setNames(
  lapply(seq_along(c("Overall", .APX_FUNDS)), function(i) {
    set.seed(i * 71 + 13)
    list(val = round(sample(40:350, 12, replace = TRUE) * 1e6),
         cnt = sample(100:1800, 12, replace = TRUE))
  }),
  c("Overall", .APX_FUNDS)
)

apx_chart_js <- paste0(
  "var apxMonths=",   jsonlite::toJSON(.apx_months),          ";",
  "var apxFundData=", jsonlite::toJSON(.apx_fund_monthly),    ";",
  "
  var _apxFundCharts = {};

  function initApxFundChart(fund) {
    if (_apxFundCharts[fund] || typeof Chart === 'undefined') return;
    var ctx = document.getElementById('apxChart-' + fund);
    if (!ctx) return;
    var d = apxFundData[fund];
    _apxFundCharts[fund] = new Chart(ctx, {
      type: 'bar',
      data: {
        labels: apxMonths,
        datasets: [{
          label: 'Unpaid Claims Value',
          data: d.val,
          backgroundColor: 'rgba(220,38,38,0.75)',
          borderColor: '#dc2626',
          borderWidth: 1,
          borderRadius: 4
        }]
      },
      options: {
        responsive: true, maintainAspectRatio: false,
        plugins: {
          legend: { display: false },
          tooltip: {
            callbacks: {
              label: function(c) {
                var v = c.parsed.y;
                return ' Value: ' + (v >= 1e9 ? 'KES ' + (v/1e9).toFixed(2) + 'B'
                       : v >= 1e6 ? 'KES ' + (v/1e6).toFixed(1) + 'M'
                       : 'KES ' + v.toLocaleString());
              },
              afterLabel: function(c) {
                return ' Count: ' + d.cnt[c.dataIndex].toLocaleString();
              }
            }
          }
        },
        scales: {
          x: {
            title: { display: true, text: 'Created Date', color: '#64748b' },
            grid: { display: false }, ticks: { color: '#64748b' }
          },
          y: {
            title: { display: true, text: 'Unpaid Claims Value (KES)', color: '#64748b' },
            grid: { color: '#f1f5f9' },
            ticks: {
              color: '#64748b',
              callback: function(v) {
                return v >= 1e9 ? (v/1e9).toFixed(1) + 'B'
                     : v >= 1e6 ? (v/1e6).toFixed(0) + 'M' : v;
              }
            }
          }
        }
      }
    });
  }

  /* Init chart when a tab becomes visible */
  document.addEventListener('shown.bs.tab', function(e) {
    var target = e.target.getAttribute('data-bs-target') || '';
    if (!target.startsWith('#apx-pane-')) return;
    setTimeout(function() { initApxFundChart(target.replace('#apx-pane-', '')); }, 50);
  });

  /* apx is the default panel - init Overall on page load */
  document.addEventListener('DOMContentLoaded', function() {
    setTimeout(function() { initApxFundChart('Overall'); }, 150);
  });

  window.revealApxCharts = function() { initApxFundChart('Overall'); };
  "
)

# ---- Component builders ------------------------------------------------------

.apx_metric_card <- function(label, value, icon_cls, color, tooltip = NULL) {
  div(class = "col",
    div(class = "card border-0 shadow-sm h-100",
      div(class = "card-body d-flex align-items-center gap-3 p-4",
        title = tooltip,
        style = if (!is.null(tooltip)) "cursor:default;" else NULL,
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
  pct_val <- round(m$unpaid_value / m$total_value * 100, 1)
  div(class = "row row-cols-1 row-cols-sm-3 g-3",
    .apx_metric_card("Total Payment Ready Claims", fmt_num(m$total_claims),
                     "bi bi-file-earmark-medical-fill", "#0284c7"),
    .apx_metric_card("Unpaid Claims",              fmt_currency(m$unpaid_value),
                     "bi bi-exclamation-circle-fill",   "#dc2626",
                     tooltip = paste0("Count: ", fmt_num(m$unpaid_claims), " claims")),
    .apx_metric_card("% of Value Unpaid",          paste0(pct_val, "%"),
                     "bi bi-percent",                   "#ea580c",
                     tooltip = paste0("Count: ", fmt_num(m$unpaid_claims), " unpaid claims"))
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
      tags$td(fmt_currency(d$total_value)),
      tags$td(fmt_currency(d$unpaid_value)),
      tags$td(pct_badge(pv, 15, 25, higher_is_bad = TRUE)),
      tags$td(fmt_num(d$total_claims)),
      tags$td(fmt_num(d$unpaid_claims)),
      tags$td(pct_badge(pc, 15, 25, higher_is_bad = TRUE))
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
        tags$td(class = "text-secondary", fmt_currency(p$total_value)),
        tags$td(class = "text-secondary", fmt_currency(p$unpaid_value)),
        tags$td(pct_badge(pvc2, 15, 25, higher_is_bad = TRUE)),
        tags$td(class = "text-secondary", fmt_num(p$total_claims)),
        tags$td(class = "text-secondary", fmt_num(p$unpaid_claims)),
        tags$td(pct_badge(pc2, 15, 25, higher_is_bad = TRUE))
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
            col_th("Total Value"),
            col_th("Unpaid Claims Value"),
            col_th("% Unpaid Value"),
            col_th("Total Claims"),
            col_th("Unpaid Claims"),
            col_th("% Unpaid Claims")
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
      # Cards
      tags$h6(class = "ind-section-label", "Summary Metrics"),
      .apx_metrics_row(metrics),

      tags$hr(class = "my-4 border-light"),

      # Chart
      tags$h6(class = "ind-section-label", "Unpaid Claims Trend"),
      div(class = "card border-0 shadow-sm mb-4",
        div(class = "card-header bg-white border-bottom px-4 py-3",
          div(class = "fw-semibold", style = "color:#0f172a;",
              "Unpaid Claims Value by Created Date"),
          div(class = "text-muted", style = "font-size:.82rem;",
              "Monthly unpaid claim value - hover to see claim count")
        ),
        div(class = "card-body px-4 py-3",
          div(style = "position:relative; height:280px;",
            tags$canvas(id = paste0("apxChart-", fund))
          )
        )
      ),

      tags$hr(class = "my-4 border-light"),

      # County table
      .apx_county_table(data, fund)
    )
  )
}

# ---- Public: panel UI --------------------------------------------------------

.apx_filter_bar <- function() {
  div(class = "card filter-card border-0 shadow-sm mb-4",
    div(class = "card-body py-3 px-4",
      div(class = "row g-3 align-items-end",

        div(class = "col-12",
          div(class = "filter-section-header",
            tags$i(class = "bi bi-sliders2"), "Filters"
          )
        ),

        # Created Date
        div(class = "col-12 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-calendar3"), "Created Date"),
            selectInput("apx_created_month", NULL,
              choices  = c("All Months" = "",
                           setNames(.apx_months, .apx_months)),
              selected = "", width = "100%")
          )
        ),

        # Incurred Date
        div(class = "col-12 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-calendar-check"), "Incurred Date"),
            selectInput("apx_incurred_month", NULL,
              choices  = c("All Months" = "",
                           setNames(.apx_months, .apx_months)),
              selected = "", width = "100%")
          )
        ),

        # County
        div(class = "col-12 col-md-3 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-geo-alt"), "County"),
            selectInput("apx_county", NULL,
              choices  = c("All Counties" = "",
                           setNames(.APX_COUNTIES, .APX_COUNTIES)),
              multiple = TRUE, selectize = TRUE, width = "100%")
          )
        ),

        # Level
        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-hospital"), "Level"),
            selectInput("apx_level", NULL,
              choices = c("All Levels" = "",
                          setNames(.APX_LEVELS, .APX_LEVELS)),
              width = "100%")
          )
        ),

        # Facility
        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-building"), "Facility"),
            selectInput("apx_facility", NULL,
              choices = c("All Facilities" = ""), width = "100%")
          )
        ),

        # Buttons
        div(class = "col-12 col-xl-2",
          div(class = "filter-bar-label", style = "visibility:hidden;", HTML("&nbsp;")),
          div(class = "d-flex gap-2 justify-content-end",
            actionButton("apx_reset",
              label = tagList(tags$i(class = "bi bi-arrow-counterclockwise"), " Reset"),
              class = "btn btn-filter-reset", title = "Reset filters"),
            actionButton("apx_apply",
              label = tagList(tags$i(class = "bi bi-funnel-fill me-1"), "Apply Filters"),
              class = "btn btn-primary btn-filter-apply")
          )
        )
      )
    )
  )
}

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

    insight_banner(
      paste0(
        fmt_num(apx_overall_metrics$unpaid_claims),
        " approved claims worth ", fmt_currency(apx_overall_metrics$unpaid_value),
        " are pending ERP disbursement across all counties."
      ),
      sub = paste0(
        round(apx_overall_metrics$unpaid_claims /
                apx_overall_metrics$total_claims * 100, 1),
        "% of approved claims remain unpaid. "
      ),
      type = "warning"
    ),

    # Fund-type tabs - full width, above filters
    tags$ul(class = "nav nav-pills nav-fill mb-3", role = "tablist",
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

    .apx_filter_bar(),

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
  observeEvent(input$apx_reset, {
    updateSelectInput(session, "apx_created_month",  selected = "")
    updateSelectInput(session, "apx_incurred_month", selected = "")
    updateSelectInput(session, "apx_county",         selected = character(0))
    updateSelectInput(session, "apx_level",          selected = "")
    updateSelectInput(session, "apx_facility",       selected = "")
  })
}
