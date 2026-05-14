# ==============================================================================
# Indicator: Claims Turnaround Time (TAT)
# Spec: /Users/danielmaangi/Downloads/claims_tat_dashboard.md
# ==============================================================================

# ---- Data --------------------------------------------------------------------

TAT_COUNTIES <- sort(c(
  "Baringo", "Bomet", "Bungoma", "Busia", "Elgeyo Marakwet", "Embu",
  "Garissa", "Homa Bay", "Isiolo", "Kajiado", "Kakamega", "Kericho",
  "Kiambu", "Kilifi", "Kirinyaga", "Kisii", "Kisumu", "Kitui", "Kwale",
  "Laikipia", "Lamu", "Machakos", "Makueni", "Mandera", "Marsabit",
  "Meru", "Migori", "Mombasa", "Murang'a", "Nairobi", "Nakuru", "Nandi",
  "Narok", "Nyamira", "Nyandarua", "Nyeri", "Samburu", "Siaya",
  "Taita-Taveta", "Tana River", "Tharaka-Nithi", "Trans Nzoia",
  "Turkana", "Uasin Gishu", "Vihiga", "Wajir", "West Pokot"
))

TAT_LEVELS <- c("Level 2", "Level 3", "Level 4", "Level 5", "National Referral")

set.seed(123)
tat_df <- data.frame(
  county     = TAT_COUNTIES,
  n_claims   = sample(300:5000, length(TAT_COUNTIES), replace = TRUE),
  min_tat    = sample(1:15,  length(TAT_COUNTIES), replace = TRUE),
  median_tat = sample(18:75, length(TAT_COUNTIES), replace = TRUE),
  max_tat    = sample(90:365, length(TAT_COUNTIES), replace = TRUE),
  pct_30     = round(runif(length(TAT_COUNTIES), 28, 72), 1),
  pct_90     = round(runif(length(TAT_COUNTIES), 60, 92), 1),
  stringsAsFactors = FALSE
)
tat_df$pct_after90 <- round(100 - tat_df$pct_90, 1)

tat_summary <- list(
  n_claims    = sum(tat_df$n_claims),
  min_tat     = min(tat_df$min_tat),
  median_tat  = as.integer(round(weighted.mean(tat_df$median_tat, tat_df$n_claims))),
  max_tat     = max(tat_df$max_tat),
  pct_30      = round(weighted.mean(tat_df$pct_30, tat_df$n_claims), 1),
  pct_90      = round(weighted.mean(tat_df$pct_90, tat_df$n_claims), 1),
  pct_after90 = round(100 - weighted.mean(tat_df$pct_90, tat_df$n_claims), 1)
)

tat_deltas <- list(
  n_claims    = +1240L, min_tat     = -2L,  median_tat  = -4L, max_tat = +18L,
  pct_30      = +3.2,   pct_90      = +1.8, pct_after90 = -1.8
)

set.seed(456)
.tat_months <- format(seq(as.Date("2025-05-01"), by = "month", length.out = 12), "%b '%y")
tat_trend <- data.frame(
  month      = .tat_months,
  min_tat    = c(9, 8, 7, 8, 6, 7, 5, 6, 5, 4, 5, 4),
  median_tat = c(38, 36, 34, 35, 32, 31, 30, 29, 28, 27, 26, 25),
  max_tat    = c(210, 205, 198, 202, 195, 190, 185, 183, 180, 178, 175, 172),
  pct_30     = c(45, 47, 48, 48, 50, 51, 52, 53, 54, 55, 56, 57),
  pct_90     = c(70, 71, 72, 72, 73, 74, 75, 76, 77, 77, 78, 79),
  n_claims   = sample(1800:2500, 12, replace = TRUE),
  stringsAsFactors = FALSE
)
tat_trend$pct_after90 <- 100 - tat_trend$pct_90

# ---- Chart.js head content ---------------------------------------------------
# Exposed as a character string; app.R injects into tags$head().

tat_chart_js <- paste0(
  # 1. Data variables
  "var tatLabels=",   jsonlite::toJSON(.tat_months),            ";",
  "var tatCounts=",   jsonlite::toJSON(tat_trend$n_claims),     ";",
  "var tatMin=",      jsonlite::toJSON(tat_trend$min_tat),      ";",
  "var tatMedian=",   jsonlite::toJSON(tat_trend$median_tat),   ";",
  "var tatMax=",      jsonlite::toJSON(tat_trend$max_tat),      ";",
  "var tatPct30=",    jsonlite::toJSON(tat_trend$pct_30),       ";",
  "var tatPct90=",    jsonlite::toJSON(tat_trend$pct_90),       ";",
  "var tatPctAfter=", jsonlite::toJSON(tat_trend$pct_after90),  ";",

  # 2. Lazy initialisation — charts are created the first time the TAT panel
  #    is revealed so Canvas elements have non-zero dimensions.
  "
  var _tatChartsReady = false;

  function initTatCharts() {
    if (_tatChartsReady || typeof Chart === 'undefined') return;

    var lineBase = { tension: 0.35, fill: false,
                     pointRadius: 4, pointHoverRadius: 6, borderWidth: 2 };

    var ctx1 = document.getElementById('tatTrendChart');
    if (ctx1) {
      new Chart(ctx1, {
        type: 'line',
        data: {
          labels: tatLabels,
          datasets: [
            Object.assign({}, lineBase, { label: 'Min TAT',    data: tatMin,
              borderColor: '#22d3ee', pointBackgroundColor: '#22d3ee' }),
            Object.assign({}, lineBase, { label: 'Median TAT', data: tatMedian,
              borderColor: '#f59e0b', pointBackgroundColor: '#f59e0b' }),
            Object.assign({}, lineBase, { label: 'Max TAT',    data: tatMax,
              borderColor: '#ef4444', pointBackgroundColor: '#ef4444' })
          ]
        },
        options: {
          responsive: true, maintainAspectRatio: false,
          interaction: { mode: 'index', intersect: false },
          plugins: {
            legend: { position: 'top',
              labels: { usePointStyle: true, pointStyleWidth: 10, boxHeight: 6 } },
            tooltip: {
              callbacks: {
                label: function(c) {
                  return ' ' + c.dataset.label + ': ' + c.parsed.y + ' days';
                },
                afterBody: function(items) {
                  return 'Claims: ' + tatCounts[items[0].dataIndex].toLocaleString();
                }
              }
            },
            annotation: {
              annotations: {
                sla30: {
                  type: 'line', yMin: 30, yMax: 30,
                  borderColor: '#22c55e', borderWidth: 2, borderDash: [6, 4],
                  label: { display: true, content: '30-day SLA', position: 'end',
                           backgroundColor: 'rgba(34,197,94,0.1)', color: '#16a34a',
                           font: { size: 11, weight: '600' }, padding: { x: 6, y: 3 } }
                },
                sla90: {
                  type: 'line', yMin: 90, yMax: 90,
                  borderColor: '#f59e0b', borderWidth: 2, borderDash: [6, 4],
                  label: { display: true, content: '90-day SLA', position: 'end',
                           backgroundColor: 'rgba(245,158,11,0.1)', color: '#b45309',
                           font: { size: 11, weight: '600' }, padding: { x: 6, y: 3 } }
                }
              }
            }
          },
          scales: {
            y: { title: { display: true, text: 'Days', color: '#64748b' },
                 grid: { color: '#f1f5f9' }, ticks: { color: '#64748b' } },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    var ctx2 = document.getElementById('tatPctChart');
    if (ctx2) {
      new Chart(ctx2, {
        type: 'line',
        data: {
          labels: tatLabels,
          datasets: [
            Object.assign({}, lineBase, { label: '% within 30 days', data: tatPct30,
              borderColor: '#22c55e', pointBackgroundColor: '#22c55e' }),
            Object.assign({}, lineBase, { label: '% within 90 days', data: tatPct90,
              borderColor: '#f59e0b', pointBackgroundColor: '#f59e0b' }),
            Object.assign({}, lineBase, { label: '% after 90 days',  data: tatPctAfter,
              borderColor: '#ef4444', pointBackgroundColor: '#ef4444' })
          ]
        },
        options: {
          responsive: true, maintainAspectRatio: false,
          interaction: { mode: 'index', intersect: false },
          plugins: {
            legend: { position: 'top',
              labels: { usePointStyle: true, pointStyleWidth: 10, boxHeight: 6 } },
            tooltip: {
              callbacks: {
                label: function(c) {
                  return ' ' + c.dataset.label + ': ' + c.parsed.y.toFixed(1) + '%';
                }
              }
            },
            annotation: {
              annotations: {
                target80: {
                  type: 'line', yMin: 80, yMax: 80,
                  borderColor: '#22c55e', borderWidth: 2, borderDash: [6, 4],
                  label: { display: true, content: '80% target (30 days)', position: 'end',
                           backgroundColor: 'rgba(34,197,94,0.1)', color: '#16a34a',
                           font: { size: 11, weight: '600' }, padding: { x: 6, y: 3 } }
                }
              }
            }
          },
          scales: {
            y: { title: { display: true, text: '%', color: '#64748b' },
                 min: 0, max: 100,
                 grid: { color: '#f1f5f9' }, ticks: { color: '#64748b' } },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    _tatChartsReady = true;
  }

  // Called by app.R nav handler when the TAT panel is revealed.
  window.revealTatCharts = initTatCharts;
  "
)

# ---- Component builders ------------------------------------------------------

.tat_metric_card <- function(label, value_str, icon_cls, color,
                             delta, lower_is_better = FALSE, suffix = "") {
  div(class = "col",
    div(class = "card border-0 shadow-sm h-100",
      div(class = "card-body p-4",
        div(class = "d-flex justify-content-between align-items-start mb-3",
          div(
            class = "rounded-3 d-flex align-items-center justify-content-center flex-shrink-0",
            style = paste0("width:44px; height:44px; background:", color, "18;"),
            tags$i(class = icon_cls,
                   style = paste0("font-size:1.2rem; color:", color, ";"))
          ),
          div(class = "text-end",
            delta_tag(delta, lower_is_better, suffix),
            tags$br(),
            tags$span(class = "text-muted", style = "font-size:.68rem;",
                      "vs prev period")
          )
        ),
        div(class = "fw-bold lh-sm mb-1",
            style = "font-size:1.5rem; color:#0f172a;", value_str),
        div(class = "text-uppercase fw-semibold text-muted",
            style = "font-size:.68rem; letter-spacing:.07em;", label)
      )
    )
  )
}

.tat_cards_ui <- function(s, d) {
  tagList(
    div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-4 g-3",
      .tat_metric_card("Number of Claims",      fmt_num(s$n_claims),
                       "bi bi-file-earmark-medical-fill", "#0284c7", d$n_claims),
      .tat_metric_card("% Paid within 30 days", paste0(s$pct_30, "%"),
                       "bi bi-check-circle-fill",         "#22c55e",
                       d$pct_30, suffix = "%"),
      .tat_metric_card("% Paid within 90 days", paste0(s$pct_90, "%"),
                       "bi bi-clock-fill",                "#f59e0b",
                       d$pct_90, suffix = "%"),
      .tat_metric_card("% Paid after 90 days",  paste0(s$pct_after90, "%"),
                       "bi bi-x-circle-fill",             "#dc2626",
                       d$pct_after90, lower_is_better = TRUE, suffix = "%")
    ),
    div(class = "row row-cols-1 row-cols-sm-3 g-3 mt-1",
      .tat_metric_card("Minimum TAT",  paste0(s$min_tat, " days"),
                       "bi bi-lightning-charge-fill",   "#22d3ee",
                       d$min_tat, lower_is_better = TRUE, suffix = " d"),
      .tat_metric_card("Median TAT",   paste0(s$median_tat, " days"),
                       "bi bi-bar-chart-line-fill",     "#f59e0b",
                       d$median_tat, lower_is_better = TRUE, suffix = " d"),
      .tat_metric_card("Maximum TAT",  paste0(s$max_tat, " days"),
                       "bi bi-exclamation-circle-fill", "#ef4444",
                       d$max_tat, lower_is_better = TRUE, suffix = " d")
    )
  )
}

.chart_card <- function(title, subtitle, canvas_id) {
  div(class = "card border-0 shadow-sm",
    div(class = "card-header bg-white border-bottom px-4 py-3",
      div(class = "fw-semibold", style = "color:#0f172a;", title),
      div(class = "text-muted", style = "font-size:.82rem;", subtitle)
    ),
    div(class = "card-body px-4 py-3",
      div(style = "position:relative; height:280px;",
        tags$canvas(id = canvas_id)
      )
    )
  )
}

.build_tat_table <- function(df) {
  if (nrow(df) == 0) {
    return(div(class = "text-center text-muted p-5",
      tags$i(class = "bi bi-funnel display-6 d-block mb-2 opacity-50"),
      "No counties match the current filters."
    ))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    tags$tr(
      tags$td(class = "fw-medium", r$county),
      tags$td(fmt_num(r$n_claims)),
      tags$td(paste0(r$min_tat,    " d")),
      tags$td(paste0(r$median_tat, " d")),
      tags$td(paste0(r$max_tat,    " d")),
      tags$td(pct_badge(r$pct_30,      40, 60, higher_is_bad = FALSE)),
      tags$td(pct_badge(r$pct_90,      70, 80, higher_is_bad = FALSE)),
      tags$td(pct_badge(r$pct_after90, 20, 30, higher_is_bad = TRUE))
    )
  })
  div(class = "table-responsive",
    tags$table(
      class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("County Name",    "180px"),
          col_th("No. of Claims"),
          col_th("Min TAT"),
          col_th("Median TAT"),
          col_th("Max TAT"),
          col_th("% within 30 d"),
          col_th("% within 90 d"),
          col_th("% after 90 d")
        )
      ),
      tags$tbody(do.call(tagList, rows))
    )
  )
}

# Horizontal filter bar (AC4) — specific to this indicator.
.tat_filter_bar <- function() {
  div(class = "card filter-card border-0 shadow-sm mb-4",
    div(class = "card-body py-3 px-4",
      div(class = "row g-3 align-items-end",

        # Section header
        div(class = "col-12",
          div(class = "filter-section-header",
            tags$i(class = "bi bi-sliders2"),
            "Filters"
          )
        ),

        # Created Date (month selector)
        div(class = "col-12 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-calendar3"), "Created Date"),
            selectInput("tat_created_month", NULL,
              choices  = c("All Months" = "",
                           setNames(.tat_months, .tat_months)),
              selected = "",
              width    = "100%")
          )
        ),

        # County
        div(class = "col-12 col-md-3 col-xl-3",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-geo-alt"), "County"),
            selectInput("tat_county", NULL,
              choices  = c("All Counties" = "",
                           setNames(TAT_COUNTIES, TAT_COUNTIES)),
              multiple = TRUE, selectize = TRUE, width = "100%")
          )
        ),

        # Level
        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-hospital"), "Level"),
            selectInput("tat_level", NULL,
              choices = c("All Levels" = "",
                          setNames(TAT_LEVELS, TAT_LEVELS)),
              width = "100%")
          )
        ),

        # Facility
        div(class = "col-6 col-md-3 col-xl-3",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-building"), "Facility"),
            selectInput("tat_facility", NULL,
              choices = c("All Facilities" = ""), width = "100%")
          )
        ),

        # Buttons
        div(class = "col-12 col-xl-2",
          div(class = "filter-bar-label", style = "visibility:hidden;", HTML("&nbsp;")),
          div(class = "d-flex gap-2 justify-content-end",
            actionButton("tat_reset",
              label = tagList(tags$i(class = "bi bi-arrow-counterclockwise"), " Reset"),
              class = "btn btn-filter-reset",
              title = "Reset filters"),
            actionButton("tat_apply",
              label = tagList(tags$i(class = "bi bi-funnel-fill me-1"), "Apply Filters"),
              class = "btn btn-primary btn-filter-apply")
          )
        )
      )
    )
  )
}

# ---- Public: panel UI --------------------------------------------------------

tat_panel_ui <- function() {
  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "Claims Turnaround Time (TAT)",
      last_updated = "13 May 2026",
      source = "Payer System",
      info = "Measures the time (in days) from claim creation to payment date. Used to monitor processing efficiency and identify bottlenecks across counties and facility levels.",
      title_suffix_id = "tat_location_suffix",
      badges = tagList(
        tags$span(class = "badge text-bg-primary px-3 py-2 rounded-pill",   "sha_user"),
        tags$span(class = "badge text-bg-secondary px-3 py-2 rounded-pill", "dha_user")
      )
    ),

    insight_banner(
      paste0(
        tat_summary$pct_after90, "% of claims (",
        fmt_num(round(tat_summary$n_claims * tat_summary$pct_after90 / 100)),
        " claims) settled beyond the 90-day SLA — median TAT is ",
        tat_summary$median_tat, " days."
      ),
      sub = paste0(
        tat_summary$pct_30, "% settled within 30 days vs an 80% benchmark. "
      ),
      type = "warning"
    ),

    div(class = "d-flex justify-content-end gap-2 mb-3",
      tags$button(type = "button",
        class = "btn btn-sm btn-outline-secondary",
        onclick = "showMockAction('Preparing TAT Excel export — the file will download shortly.')",
        tags$i(class = "bi bi-file-earmark-excel me-1"), "Export"
      )
    ),

    # AC4 — filters (horizontal bar, indicator-specific)
    .tat_filter_bar(),

    # AC1 — Summary cards
    tags$h6(class = "ind-section-label", "Summary Metrics"),
    .tat_cards_ui(tat_summary, tat_deltas),

    tags$hr(class = "my-4 border-light"),

    # AC2 — Monthly trend charts
    tags$h6(class = "ind-section-label", "Monthly Trend (last 12 months)"),
    div(class = "row g-4",
      div(class = "col-12 col-xl-6",
        .chart_card("TAT Trends",
                    "Min / Median / Max days — dashed lines show 30-day and 90-day SLA targets",
                    "tatTrendChart")),
      div(class = "col-12 col-xl-6",
        .chart_card("Payment Period Distribution",
                    "Share of claims by settlement window — dashed line marks the 80% target at 30 days",
                    "tatPctChart"))
    ),

    tags$hr(class = "my-4 border-light"),

    # AC3 — County comparison table
    tags$h6(class = "ind-section-label", "County Comparison"),
    div(class = "card border-0 shadow-sm",
      div(class = "card-header bg-white border-bottom d-flex justify-content-between align-items-center py-3 px-4",
        div(class = "fw-semibold", style = "color:#0f172a;", "TAT by County"),
        div(class = "d-flex align-items-center gap-2",
          uiOutput("tat_row_count"),
          tags$button(type = "button",
            class = "btn btn-sm btn-outline-secondary",
            onclick = "showMockAction('Preparing TAT county Excel export — the file will download shortly.')",
            tags$i(class = "bi bi-file-earmark-excel me-1"), "Export"
          )
        )
      ),
      uiOutput("tat_table_ui"),
      div(class = "card-footer bg-white border-top d-flex justify-content-between align-items-center py-2 px-4",
        uiOutput("tat_showing"),
        uiOutput("tat_pagination")
      )
    )
  )
}

# ---- Public: server ----------------------------------------------------------

tat_server <- function(input, output, session) {
  PAGE_SIZE <- 10L
  tat_page  <- reactiveVal(1L)

  filtered_df <- eventReactive(input$tat_apply, {
    df  <- tat_df
    sel <- input$tat_county
    if (!is.null(sel) && length(sel) > 0 && any(nchar(sel) > 0))
      df <- df[df$county %in% sel, ]
    df
  }, ignoreNULL = FALSE)

  observeEvent(input$tat_apply, { tat_page(1L) })
  observeEvent(input$tat_page_num, { tat_page(as.integer(input$tat_page_num)) })

  total_pages <- reactive({ max(1L, ceiling(nrow(filtered_df()) / PAGE_SIZE)) })

  paged_df <- reactive({
    df <- filtered_df()
    if (nrow(df) == 0L) return(df)
    p   <- min(tat_page(), total_pages())
    idx <- seq.int((p - 1L) * PAGE_SIZE + 1L, min(p * PAGE_SIZE, nrow(df)))
    df[idx, , drop = FALSE]
  })

  output$tat_table_ui <- renderUI({ .build_tat_table(paged_df()) })

  output$tat_row_count <- renderUI({
    n  <- nrow(filtered_df())
    p  <- min(tat_page(), total_pages())
    s  <- (p - 1L) * PAGE_SIZE + 1L
    e  <- min(p * PAGE_SIZE, n)
    tags$span(class = "badge text-bg-light border small",
              if (n == 0) "0 counties" else sprintf("%d–%d of %d counties", s, e, n))
  })

  output$tat_showing <- renderUI({
    n <- nrow(filtered_df())
    tags$span(class = "text-muted small",
      tags$i(class = "bi bi-info-circle me-1"),
      sprintf("%d of %d counties shown", n, nrow(tat_df)))
  })

  output$tat_pagination <- renderUI({
    make_pagination(min(tat_page(), total_pages()), total_pages(), "tat_page_num")
  })

  output$tat_location_suffix <- renderUI({
    sel <- input$tat_county
    loc <- if (is.null(sel) || length(sel) == 0) "Kenya" else paste(sel, collapse = ", ")
    tags$span(style = "color:#94a3b8; font-weight:400; font-size:1rem;",
              paste0("| ", loc))
  })

  observeEvent(input$tat_reset, {
    updateSelectInput(session, "tat_created_month", selected = "")
    updateSelectInput(session, "tat_county",        selected = character(0))
    updateSelectInput(session, "tat_level",         selected = "")
    updateSelectInput(session, "tat_facility",      selected = "")
    tat_page(1L)
  })
}
