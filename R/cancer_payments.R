# ==============================================================================
# Indicator: Cancer Patients & SHA Chemotherapy Payments
# Spec: requirements/cancer_patients_sha_payments_dashboard.md
# Data source: SHA-06 (21–33) Hemato-Oncology Treatment; patient ID = CR Number
# ==============================================================================

# ---- Data --------------------------------------------------------------------

CANCER_COUNTIES <- sort(c(
  "Baringo", "Bomet", "Bungoma", "Busia", "Elgeyo Marakwet", "Embu",
  "Garissa", "Homa Bay", "Isiolo", "Kajiado", "Kakamega", "Kericho",
  "Kiambu", "Kilifi", "Kirinyaga", "Kisii", "Kisumu", "Kitui", "Kwale",
  "Laikipia", "Lamu", "Machakos", "Makueni", "Mandera", "Marsabit",
  "Meru", "Migori", "Mombasa", "Murang'a", "Nairobi", "Nakuru", "Nandi",
  "Narok", "Nyamira", "Nyandarua", "Nyeri", "Samburu", "Siaya",
  "Taita-Taveta", "Tana River", "Tharaka-Nithi", "Trans Nzoia",
  "Turkana", "Uasin Gishu", "Vihiga", "Wajir", "West Pokot"
))

CANCER_LEVELS <- c("Level 2", "Level 3", "Level 4", "Level 5", "National Referral")

set.seed(77)
cancer_county_df <- data.frame(
  county          = CANCER_COUNTIES,
  total_patients  = sample(40:800,  length(CANCER_COUNTIES), replace = TRUE),
  new_patients    = sample(5:120,   length(CANCER_COUNTIES), replace = TRUE),
  amount_paid     = runif(length(CANCER_COUNTIES), 2e5, 8e6),
  stringsAsFactors = FALSE
)

cancer_summary <- list(
  unique_3m    = as.integer(round(sum(cancer_county_df$total_patients) * 0.72)),
  new_month    = sum(cancer_county_df$new_patients),
  amount_paid  = sum(cancer_county_df$amount_paid)
)

cancer_deltas <- list(
  unique_3m   = +143L,
  new_month   = +28L,
  amount_paid = +1.24e6
)

# Monthly trend (12 months)
set.seed(88)
.cancer_months <- format(
  seq(as.Date("2025-05-01"), by = "month", length.out = 12), "%b '%y"
)

cancer_trend <- data.frame(
  month           = .cancer_months,
  unique_patients = c(920, 945, 970, 958, 990, 1020, 1045, 1060, 1080, 1100, 1125, 1148),
  new_patients    = c(112, 118, 105, 121, 130, 115,  128,  140,  132,  145,  138,  152),
  total_amount    = c(4.1, 4.3, 4.2, 4.5, 4.6, 4.8, 5.0, 5.1, 5.3, 5.5, 5.6, 5.8) * 1e6,
  new_amount      = c(0.9, 1.0, 0.95, 1.05, 1.1, 1.0, 1.15, 1.2, 1.1, 1.25, 1.2, 1.35) * 1e6,
  stringsAsFactors = FALSE
)

# ---- Chart.js head content ---------------------------------------------------

cancer_chart_js <- paste0(
  "var cncLabels=",      jsonlite::toJSON(.cancer_months),                ";",
  "var cncUnique=",      jsonlite::toJSON(cancer_trend$unique_patients),  ";",
  "var cncNew=",         jsonlite::toJSON(cancer_trend$new_patients),     ";",
  "var cncTotalAmt=",    jsonlite::toJSON(cancer_trend$total_amount / 1e6), ";",
  "var cncNewAmt=",      jsonlite::toJSON(cancer_trend$new_amount / 1e6),   ";",

  "
  var _cncChartsReady = false;

  function initCancerCharts() {
    if (_cncChartsReady || typeof Chart === 'undefined') return;

    var lineBase = { tension: 0.35, fill: false,
                     pointRadius: 4, pointHoverRadius: 6, borderWidth: 2 };

    /* ---- Chart 1: Patient Volume ---- */
    var ctx1 = document.getElementById('cncVolumeChart');
    if (ctx1) {
      new Chart(ctx1, {
        type: 'line',
        data: {
          labels: cncLabels,
          datasets: [
            Object.assign({}, lineBase, {
              label: 'Unique patients',
              data: cncUnique,
              borderColor: '#7c3aed', pointBackgroundColor: '#7c3aed'
            }),
            Object.assign({}, lineBase, {
              label: 'New patients',
              data: cncNew,
              borderColor: '#ec4899', pointBackgroundColor: '#ec4899',
              borderDash: [5, 3]
            })
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
                afterBody: function(items) {
                  var idx = items[0].dataIndex;
                  var pct = ((cncNew[idx] / cncUnique[idx]) * 100).toFixed(1);
                  return 'New as % of unique: ' + pct + '%';
                }
              }
            }
          },
          scales: {
            y: { title: { display: true, text: 'Patients', color: '#64748b' },
                 grid: { color: '#f1f5f9' }, ticks: { color: '#64748b' } },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    /* ---- Chart 2: Payment Amount ---- */
    var ctx2 = document.getElementById('cncAmountChart');
    if (ctx2) {
      new Chart(ctx2, {
        type: 'line',
        data: {
          labels: cncLabels,
          datasets: [
            Object.assign({}, lineBase, {
              label: 'Total amount (KES M)',
              data: cncTotalAmt,
              borderColor: '#0891b2', pointBackgroundColor: '#0891b2'
            }),
            Object.assign({}, lineBase, {
              label: 'New patients amount (KES M)',
              data: cncNewAmt,
              borderColor: '#f59e0b', pointBackgroundColor: '#f59e0b',
              borderDash: [5, 3]
            })
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
                  return ' ' + c.dataset.label + ': KES ' + c.parsed.y.toFixed(2) + 'M';
                },
                afterBody: function(items) {
                  var idx = items[0].dataIndex;
                  var pct = ((cncNewAmt[idx] / cncTotalAmt[idx]) * 100).toFixed(1);
                  return 'New patients share: ' + pct + '%';
                }
              }
            }
          },
          scales: {
            y: { title: { display: true, text: 'KES (M)', color: '#64748b' },
                 grid: { color: '#f1f5f9' }, ticks: { color: '#64748b' } },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    _cncChartsReady = true;
  }

  window.revealCancerCharts = initCancerCharts;
  "
)

# ---- Component builders ------------------------------------------------------

.cnc_metric_card <- function(label, value_str, icon_cls, color,
                             delta, delta_fmt = fmt_num) {
  delta_val  <- delta_fmt(abs(delta))
  is_pos     <- delta > 0
  color_d    <- if (is_pos) "#16a34a" else "#dc2626"
  arrow      <- if (is_pos) "bi-arrow-up-short" else "bi-arrow-down-short"
  sign       <- if (is_pos) "+" else "-"

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
            tags$span(
              style = paste0("font-size:.75rem; font-weight:600; color:", color_d, ";"),
              tags$i(class = paste("bi", arrow)),
              paste0(sign, delta_val)
            ),
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

.cnc_cards_ui <- function(s, d) {
  div(class = "row row-cols-1 row-cols-sm-3 g-3",
    .cnc_metric_card(
      "Unique patients treated (last 3 months)",
      fmt_num(s$unique_3m),
      "bi bi-people-fill", "#7c3aed",
      d$unique_3m
    ),
    .cnc_metric_card(
      "New patients treated this month",
      fmt_num(s$new_month),
      "bi bi-person-plus-fill", "#ec4899",
      d$new_month
    ),
    .cnc_metric_card(
      "Amount paid by SHA",
      fmt_currency(s$amount_paid),
      "bi bi-cash-stack", "#0891b2",
      d$amount_paid, delta_fmt = fmt_currency
    )
  )
}

.cnc_chart_card <- function(title, subtitle, canvas_id) {
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

.build_cnc_table <- function(df) {
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
      tags$td(fmt_num(r$total_patients)),
      tags$td(fmt_num(r$new_patients)),
      tags$td(fmt_currency(r$amount_paid))
    )
  })
  div(class = "table-responsive",
    tags$table(
      class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("County Name",         "180px"),
          col_th("Total Cancer Patients"),
          col_th("New Patients"),
          col_th("Amount SHA Paid")
        )
      ),
      tags$tbody(do.call(tagList, rows))
    )
  )
}

.cnc_filter_bar <- function() {
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
            selectInput("cnc_created_month", NULL,
              choices  = c("All Months" = "",
                           setNames(.cancer_months, .cancer_months)),
              selected = "",
              width    = "100%")
          )
        ),

        # County
        div(class = "col-12 col-md-3 col-xl-3",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-geo-alt"), "County"),
            selectInput("cnc_county", NULL,
              choices  = c("All Counties" = "",
                           setNames(CANCER_COUNTIES, CANCER_COUNTIES)),
              multiple = TRUE, selectize = TRUE, width = "100%")
          )
        ),

        # Level
        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-hospital"), "Level"),
            selectInput("cnc_level", NULL,
              choices = c("All Levels" = "",
                          setNames(CANCER_LEVELS, CANCER_LEVELS)),
              width = "100%")
          )
        ),

        # Facility
        div(class = "col-6 col-md-3 col-xl-3",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-building"), "Facility"),
            selectInput("cnc_facility", NULL,
              choices = c("All Facilities" = ""), width = "100%")
          )
        ),

        # Buttons
        div(class = "col-12 col-xl-2",
          div(class = "filter-bar-label", style = "visibility:hidden;", HTML("&nbsp;")),
          div(class = "d-flex gap-2 justify-content-end",
            actionButton("cnc_reset",
              label = tagList(tags$i(class = "bi bi-arrow-counterclockwise"), " Reset"),
              class = "btn btn-filter-reset",
              title = "Reset filters"),
            actionButton("cnc_apply",
              label = tagList(tags$i(class = "bi bi-funnel-fill me-1"), "Apply Filters"),
              class = "btn btn-primary btn-filter-apply")
          )
        )
      )
    )
  )
}

# ---- Public: panel UI --------------------------------------------------------

cancer_panel_ui <- function() {
  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "SHA Payments for Cancer Treatment",
      last_updated = "13 May 2026",
      source = "Payer System",
      info = "Tracks SHA Hemato-Oncology (SHA-06, benefit codes 21–33) treatment payments. Each patient is identified by their CR Number. Covers unique patients, new patients, and total amounts paid by SHA.",
      title_suffix_id = "cnc_location_suffix",
      badges = tagList(
        tags$span(class = "badge text-bg-success px-3 py-2 rounded-pill", "all_users")
      )
    ),

    # AC4 — filters
    .cnc_filter_bar(),

    # AC1 — Summary cards
    tags$h6(class = "ind-section-label", "Summary Metrics"),
    .cnc_cards_ui(cancer_summary, cancer_deltas),

    tags$hr(class = "my-4 border-light"),

    # AC2 — Monthly trend charts
    tags$h6(class = "ind-section-label", "Monthly Trend (last 12 months)"),
    div(class = "row g-4",
      div(class = "col-12 col-xl-6",
        .cnc_chart_card(
          "Cancer Patient Trends",
          "Unique patients vs new patients SHA paid for",
          "cncVolumeChart"
        )
      ),
      div(class = "col-12 col-xl-6",
        .cnc_chart_card(
          "Payment Amount Trends",
          "Total amount vs amount paid for new patients (KES M)",
          "cncAmountChart"
        )
      )
    ),

    tags$hr(class = "my-4 border-light"),

    # AC3 — County comparison table
    tags$h6(class = "ind-section-label", "County Comparison"),
    div(class = "card border-0 shadow-sm",
      div(class = "card-header bg-white border-bottom d-flex justify-content-between align-items-center py-3 px-4",
        div(class = "fw-semibold", style = "color:#0f172a;",
            "Cancer Patients & Payments by County"),
        uiOutput("cnc_row_count")
      ),
      uiOutput("cnc_table_ui"),
      div(class = "card-footer bg-white border-top d-flex justify-content-between align-items-center py-2 px-4",
        uiOutput("cnc_showing"),
        uiOutput("cnc_pagination")
      )
    )
  )
}

# ---- Public: server ----------------------------------------------------------

cancer_server <- function(input, output, session) {
  PAGE_SIZE <- 10L
  cnc_page  <- reactiveVal(1L)

  filtered_df <- eventReactive(input$cnc_apply, {
    df  <- cancer_county_df
    sel <- input$cnc_county
    if (!is.null(sel) && length(sel) > 0 && any(nchar(sel) > 0))
      df <- df[df$county %in% sel, ]
    df
  }, ignoreNULL = FALSE)

  observeEvent(input$cnc_apply, { cnc_page(1L) })
  observeEvent(input$cnc_page_num, { cnc_page(as.integer(input$cnc_page_num)) })

  total_pages <- reactive({ max(1L, ceiling(nrow(filtered_df()) / PAGE_SIZE)) })

  paged_df <- reactive({
    df <- filtered_df()
    if (nrow(df) == 0L) return(df)
    p   <- min(cnc_page(), total_pages())
    idx <- seq.int((p - 1L) * PAGE_SIZE + 1L, min(p * PAGE_SIZE, nrow(df)))
    df[idx, , drop = FALSE]
  })

  output$cnc_table_ui <- renderUI({ .build_cnc_table(paged_df()) })

  output$cnc_row_count <- renderUI({
    n  <- nrow(filtered_df())
    p  <- min(cnc_page(), total_pages())
    s  <- (p - 1L) * PAGE_SIZE + 1L
    e  <- min(p * PAGE_SIZE, n)
    tags$span(class = "badge text-bg-light border small",
              if (n == 0) "0 counties" else sprintf("%d–%d of %d counties", s, e, n))
  })

  output$cnc_showing <- renderUI({
    n <- nrow(filtered_df())
    tags$span(class = "text-muted small",
      tags$i(class = "bi bi-info-circle me-1"),
      sprintf("%d of %d counties shown", n, nrow(cancer_county_df)))
  })

  output$cnc_pagination <- renderUI({
    make_pagination(min(cnc_page(), total_pages()), total_pages(), "cnc_page_num")
  })

  output$cnc_location_suffix <- renderUI({
    sel <- input$cnc_county
    loc <- if (is.null(sel) || length(sel) == 0) "Kenya" else paste(sel, collapse = ", ")
    tags$span(style = "color:#94a3b8; font-weight:400; font-size:1rem;",
              paste0("| ", loc))
  })

  observeEvent(input$cnc_reset, {
    updateSelectInput(session, "cnc_created_month", selected = "")
    updateSelectInput(session, "cnc_county",        selected = character(0))
    updateSelectInput(session, "cnc_level",         selected = "")
    updateSelectInput(session, "cnc_facility",      selected = "")
    cnc_page(1L)
  })
}
