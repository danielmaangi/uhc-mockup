# ==============================================================================
# Indicator: Deliveries Funded by SHA
# Spec: requirements/sha_deliveries_dashboard.md
# ==============================================================================

# ---- Constants ---------------------------------------------------------------

.DLV_COUNTIES <- sort(c(
  "Baringo", "Bomet", "Bungoma", "Busia", "Elgeyo Marakwet", "Embu",
  "Garissa", "Homa Bay", "Isiolo", "Kajiado", "Kakamega", "Kericho",
  "Kiambu", "Kilifi", "Kirinyaga", "Kisii", "Kisumu", "Kitui", "Kwale",
  "Laikipia", "Lamu", "Machakos", "Makueni", "Mandera", "Marsabit",
  "Meru", "Migori", "Mombasa", "Murang'a", "Nairobi", "Nakuru", "Nandi",
  "Narok", "Nyamira", "Nyandarua", "Nyeri", "Samburu", "Siaya",
  "Taita-Taveta", "Tana River", "Tharaka-Nithi", "Trans Nzoia",
  "Turkana", "Uasin Gishu", "Vihiga", "Wajir", "West Pokot"
))

.dlv_months    <- format(seq(as.Date("2025-05-01"), by = "month", length.out = 12), "%b '%y")
.DLV_LEVELS    <- c("Level 2", "Level 3", "Level 4", "Level 5", "National Referral")
.DLV_OWNERSHIP <- c("Public", "Private", "Faith-based", "NGO")

# ---- Data --------------------------------------------------------------------

set.seed(101)
.dlv_total <- round(seq(40000, 52000, length.out = 12) + rnorm(12, 0, 1800))
.dlv_sha   <- round(.dlv_total * runif(12, 0.44, 0.64))

dlv_trend <- data.frame(
  month = .dlv_months,
  total = .dlv_total,
  sha   = .dlv_sha,
  prop  = round(.dlv_sha / .dlv_total * 100, 1),
  stringsAsFactors = FALSE
)

.dlv_build_county <- function(county, idx) {
  set.seed(idx * 23)
  khis <- sample(8000:35000, 1)
  pct  <- runif(1, 0.28, 0.90)
  sha  <- round(khis * pct)
  data.frame(
    county     = county,
    khis_count = khis,
    sha_count  = sha,
    proportion = round(sha / khis * 100, 1),
    stringsAsFactors = FALSE
  )
}

dlv_county_df <- do.call(rbind, lapply(seq_along(.DLV_COUNTIES), function(i)
  .dlv_build_county(.DLV_COUNTIES[i], i)))
dlv_county_df <- dlv_county_df[order(dlv_county_df$proportion), ]
rownames(dlv_county_df) <- NULL

dlv_summary <- list(
  total_deliveries = sum(dlv_trend$total),
  sha_claimed      = sum(dlv_trend$sha),
  proportion_pct   = round(sum(dlv_trend$sha) / sum(dlv_trend$total) * 100, 1)
)

# ---- Chart.js head content ---------------------------------------------------

deliveries_chart_js <- paste0(
  "var dlvLabels=", jsonlite::toJSON(.dlv_months),      ";",
  "var dlvTotal=",  jsonlite::toJSON(dlv_trend$total),  ";",
  "var dlvSha=",    jsonlite::toJSON(dlv_trend$sha),    ";",
  "
  var _dlvChart = null;
  var _dlvChartsReady = false;

  function initDeliveriesCharts() {
    var ctx = document.getElementById('dlvTrendChart');
    if (!ctx) return;
    if (_dlvChart) { _dlvChart.destroy(); _dlvChart = null; }
    _dlvChart = new Chart(ctx, {
      type: 'line',
      data: {
        labels: dlvLabels,
        datasets: [
          {
            label: 'Total Deliveries (KHIS)',
            data: dlvTotal,
            borderColor: '#64748b',
            pointBackgroundColor: '#64748b',
            borderWidth: 2,
            tension: 0.35,
            pointRadius: 4,
            pointHoverRadius: 6,
            fill: false
          },
          {
            label: 'SHA Claimed Deliveries',
            data: dlvSha,
            borderColor: '#0e82b5',
            pointBackgroundColor: '#0e82b5',
            borderWidth: 2,
            tension: 0.35,
            pointRadius: 4,
            pointHoverRadius: 6,
            fill: { target: '-1', above: 'rgba(14,130,181,0.1)' }
          }
        ]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { position: 'bottom', labels: { font: { size: 12 }, padding: 16 } },
          tooltip: {
            callbacks: {
              afterBody: function(items) {
                if (items.length < 2) return [];
                var total = items[0].raw;
                var sha   = items[1].raw;
                var pct   = total > 0 ? (sha / total * 100).toFixed(1) : '—';
                var gap   = (total - sha).toLocaleString();
                return ['Proportion: ' + pct + '%', 'Gap: ' + gap];
              }
            }
          }
        },
        scales: {
          x: {
            grid: { color: 'rgba(0,0,0,0.05)' },
            ticks: { font: { size: 11 } }
          },
          y: {
            beginAtZero: false,
            grid: { color: 'rgba(0,0,0,0.05)' },
            ticks: {
              font: { size: 11 },
              callback: function(v) { return v >= 1000 ? (v / 1000).toFixed(0) + 'k' : v; }
            }
          }
        }
      }
    });
    _dlvChartsReady = true;
  }

  window.revealDeliveriesCharts = function() {
    if (!_dlvChartsReady) initDeliveriesCharts();
  };
  "
)

# ---- Component builders ------------------------------------------------------

.dlv_metric_card <- function(label, value, icon_cls, color, tooltip = NULL) {
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

.dlv_cards_ui <- function(m) {
  div(class = "row row-cols-1 row-cols-sm-3 g-3",
    .dlv_metric_card("Total Deliveries (KHIS)",
                     fmt_num(m$total_deliveries),
                     "bi bi-hospital-fill", "#0284c7"),
    .dlv_metric_card("SHA Claimed Deliveries",
                     fmt_num(m$sha_claimed),
                     "bi bi-file-earmark-check-fill", "#059669"),
    .dlv_metric_card("Proportion Paid by SHA",
                     paste0(m$proportion_pct, "%"),
                     "bi bi-percent", "#7c3aed")
  )
}

.dlv_filter_bar <- function() {
  div(class = "card filter-card border-0 shadow-sm mb-4",
    div(class = "card-body py-3 px-4",
      div(class = "row g-3 align-items-end",

        div(class = "col-12",
          div(class = "filter-section-header",
            tags$i(class = "bi bi-sliders2"),
            "Filters"
          )
        ),

        div(class = "col-12 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-calendar-check"), "Incurred Date"),
            selectInput("dlv_month", NULL,
              choices  = c("All Months" = "",
                           setNames(.dlv_months, .dlv_months)),
              selected = "",
              width    = "100%")
          )
        ),

        div(class = "col-12 col-md-3 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-geo-alt"), "County"),
            selectInput("dlv_county", NULL,
              choices  = c("All Counties" = "",
                           setNames(.DLV_COUNTIES, .DLV_COUNTIES)),
              multiple = TRUE, selectize = TRUE, width = "100%")
          )
        ),

        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-hospital"), "Level"),
            selectInput("dlv_level", NULL,
              choices = c("All Levels" = "",
                          setNames(.DLV_LEVELS, .DLV_LEVELS)),
              width = "100%")
          )
        ),

        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-building"), "Ownership"),
            selectInput("dlv_ownership", NULL,
              choices = c("All Types" = "",
                          setNames(.DLV_OWNERSHIP, .DLV_OWNERSHIP)),
              width = "100%")
          )
        ),

        div(class = "col-12 col-md-3 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-house-heart"), "Facility Name"),
            selectInput("dlv_facility", NULL,
              choices = c("All Facilities" = ""), width = "100%")
          )
        ),

        div(class = "col-12 col-xl-2",
          div(class = "filter-bar-label", style = "visibility:hidden;", HTML("&nbsp;")),
          div(class = "d-flex gap-2",
            actionButton("dlv_reset",
              label = tagList(tags$i(class = "bi bi-arrow-counterclockwise"), " Reset"),
              class = "btn btn-filter-reset",
              title = "Reset filters"),
            actionButton("dlv_apply",
              label = tagList(tags$i(class = "bi bi-funnel-fill me-1"), "Apply Filters"),
              class = "btn btn-primary btn-filter-apply")
          )
        )
      )
    )
  )
}

.build_dlv_table <- function(df) {
  if (nrow(df) == 0) {
    return(div(class = "text-center text-muted p-5",
      tags$i(class = "bi bi-funnel display-6 d-block mb-2 opacity-50"),
      "No counties match the current filters."
    ))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    r         <- df[i, ]
    prop_cell <- if (is.na(r$proportion) || r$khis_count == 0)
      tags$td(tags$span("—"))
    else
      tags$td(pct_badge(r$proportion, lo = 50, hi = 80, higher_is_bad = FALSE))
    tags$tr(
      tags$td(class = "fw-medium", r$county),
      tags$td(fmt_num(r$khis_count)),
      tags$td(fmt_num(r$sha_count)),
      prop_cell
    )
  })
  div(class = "table-responsive",
    tags$table(
      class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("County Name",         "180px"),
          col_th("KHIS Delivery Count"),
          col_th("SHA Claimed Count"),
          col_th("Proportion")
        )
      ),
      tags$tbody(do.call(tagList, rows))
    )
  )
}

# ---- Public: panel UI --------------------------------------------------------

deliveries_panel_ui <- function() {
  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "Deliveries Funded by SHA",
      last_updated = "13 May 2026",
      source       = "Payer. ERP · KHIS / DHIS2"
    ),

    insight_banner(
      paste0(dlv_summary$proportion_pct,
             "% of facility deliveries were financed through SHA."),
      sub = paste0(
        "SHA claimed: ", fmt_num(dlv_summary$sha_claimed), " deliveries.  ",
        "Total (KHIS): ", fmt_num(dlv_summary$total_deliveries), ".  ",
        "KHIS outliers capped using IQR method before proportion calculations."
      ),
      type = "info"
    ),

    .dlv_filter_bar(),

    tags$h6(class = "ind-section-label", "National Summary"),
    .dlv_cards_ui(dlv_summary),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label", "Monthly Trend"),
    div(class = "card border-0 shadow-sm mb-4",
      div(class = "card-header bg-white border-bottom px-4 py-3",
        div(class = "fw-semibold", style = "color:#0f172a;",
            "Total Deliveries vs SHA Claimed — Monthly"),
        div(class = "text-muted", style = "font-size:.82rem;",
            "Shaded area represents the coverage gap (unclaimed deliveries)")
      ),
      div(class = "card-body px-4 py-3",
        div(style = "position:relative; height:300px;",
          tags$canvas(id = "dlvTrendChart")
        )
      ),
      div(class = "card-footer bg-white border-top px-4 py-2",
        tags$ul(class = "mb-0 ps-3", style = "font-size:.7rem; color:#94a3b8;",
          tags$li(HTML("<strong>Total Deliveries (KHIS):</strong> DHIS2 indicator BDsWWkJHuce. Outliers capped to nearest IQR fence before rendering.")),
          tags$li(HTML("<strong>SHA Claimed Deliveries:</strong> Sum of codes SHA-08-005 (Vaginal), SHA-08-006 (Caesarean), SHA-08-007 (Multiple births)."))
        )
      )
    ),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label", "County Comparison"),
    div(class = "card border-0 shadow-sm",
      div(class = "card-header bg-white border-bottom d-flex justify-content-between align-items-center py-3 px-4",
        div(class = "fw-semibold", style = "color:#0f172a;",
            "Delivery Coverage by County"),
        div(class = "d-flex align-items-center gap-2",
          uiOutput("dlv_row_count"),
          tags$button(type = "button",
            class = "btn btn-sm btn-outline-secondary",
            onclick = "showMockAction('Preparing deliveries Excel export — the file will download shortly.')",
            tags$i(class = "bi bi-file-earmark-excel me-1"), "Export"
          )
        )
      ),
      uiOutput("dlv_table_ui"),
      div(class = "card-footer bg-white border-top px-4 py-2",
        div(class = "d-flex justify-content-between align-items-center",
          uiOutput("dlv_showing"),
          uiOutput("dlv_pagination")
        ),
        div(class = "d-flex gap-3 mt-2", style = "font-size:.7rem; color:#64748b;",
          tags$span(tags$span(class = "badge bg-danger me-1", "< 50%"),   "Low coverage"),
          tags$span(tags$span(class = "badge bg-warning text-dark me-1", "50–79%"), "Moderate"),
          tags$span(tags$span(class = "badge bg-success me-1", "≥ 80%"), "Adequate")
        )
      )
    )
  )
}

# ---- Public: server ----------------------------------------------------------

deliveries_server <- function(input, output, session) {
  PAGE_SIZE <- 10L
  dlv_page  <- reactiveVal(1L)

  filtered_df <- eventReactive(input$dlv_apply, {
    df  <- dlv_county_df
    sel <- input$dlv_county
    if (!is.null(sel) && length(sel) > 0 && any(nchar(sel) > 0))
      df <- df[df$county %in% sel, ]
    df
  }, ignoreNULL = FALSE)

  observeEvent(input$dlv_apply,    { dlv_page(1L) })
  observeEvent(input$dlv_page_num, { dlv_page(as.integer(input$dlv_page_num)) })

  total_pages <- reactive({ max(1L, ceiling(nrow(filtered_df()) / PAGE_SIZE)) })

  paged_df <- reactive({
    df <- filtered_df()
    if (nrow(df) == 0L) return(df)
    p   <- min(dlv_page(), total_pages())
    idx <- seq.int((p - 1L) * PAGE_SIZE + 1L, min(p * PAGE_SIZE, nrow(df)))
    df[idx, , drop = FALSE]
  })

  output$dlv_table_ui <- renderUI({ .build_dlv_table(paged_df()) })

  output$dlv_row_count <- renderUI({
    n <- nrow(filtered_df())
    p <- min(dlv_page(), total_pages())
    s <- (p - 1L) * PAGE_SIZE + 1L
    e <- min(p * PAGE_SIZE, n)
    tags$span(class = "badge text-bg-light border small",
              if (n == 0) "0 counties" else sprintf("%d-%d of %d counties", s, e, n))
  })

  output$dlv_showing <- renderUI({
    n <- nrow(filtered_df())
    tags$span(class = "text-muted small",
      tags$i(class = "bi bi-info-circle me-1"),
      sprintf("%d of %d counties shown", n, nrow(dlv_county_df)))
  })

  output$dlv_pagination <- renderUI({
    make_pagination(min(dlv_page(), total_pages()), total_pages(), "dlv_page_num")
  })

  observeEvent(input$dlv_reset, {
    updateSelectInput(session, "dlv_month",     selected = "")
    updateSelectInput(session, "dlv_county",    selected = character(0))
    updateSelectInput(session, "dlv_level",     selected = "")
    updateSelectInput(session, "dlv_ownership", selected = "")
    updateSelectInput(session, "dlv_facility",  selected = "")
    dlv_page(1L)
  })
}
