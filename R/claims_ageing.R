# ==============================================================================
# Indicator: Claims Ageing Report
# Spec: requirements/claims_ageing_report.md
# ==============================================================================

# ---- Constants ---------------------------------------------------------------

.AG_BUCKET_LABELS <- c("0-30 days", "31-60 days", "61-90 days", "90+ days")
.AG_BUCKET_COLORS <- c("#22c55e", "#f59e0b", "#f97316", "#ef4444")
.AG_LEVELS        <- c("Level 2", "Level 3", "Level 4", "Level 5", "National Referral")
.ag_months        <- format(seq(as.Date("2025-05-01"), by = "month", length.out = 12), "%b '%y")

# ---- Data --------------------------------------------------------------------

.AG_COUNTIES <- sort(c(
  "Baringo", "Bomet", "Bungoma", "Busia", "Elgeyo Marakwet", "Embu",
  "Garissa", "Homa Bay", "Isiolo", "Kajiado", "Kakamega", "Kericho",
  "Kiambu", "Kilifi", "Kirinyaga", "Kisii", "Kisumu", "Kitui", "Kwale",
  "Laikipia", "Lamu", "Machakos", "Makueni", "Mandera", "Marsabit",
  "Meru", "Migori", "Mombasa", "Murang'a", "Nairobi", "Nakuru", "Nandi",
  "Narok", "Nyamira", "Nyandarua", "Nyeri", "Samburu", "Siaya",
  "Taita-Taveta", "Tana River", "Tharaka-Nithi", "Trans Nzoia",
  "Turkana", "Uasin Gishu", "Vihiga", "Wajir", "West Pokot"
))

.ag_build_county <- function(county, idx) {
  set.seed(idx * 17)
  n_prov  <- sample(2:3, 1)
  avg_val <- runif(1, 18000, 75000)

  prov_rows <- lapply(seq_len(n_prov), function(j) {
    set.seed(idx * 100 + j * 31)
    b <- c(sample(80:600, 1), sample(40:250, 1), sample(15:100, 1), sample(5:55, 1))
    v <- b * avg_val * runif(4, 0.85, 1.15)
    pname <- switch(j,
      paste(county, "Level 5 Hospital"),
      paste(county, "Sub-County Hospital"),
      paste(county, "Health Centre")
    )
    data.frame(
      name   = pname,
      b0_30  = b[1], b31_60 = b[2], b61_90 = b[3], b90p = b[4],
      v0_30  = v[1], v31_60 = v[2], v61_90 = v[3], v90p = v[4],
      stringsAsFactors = FALSE
    )
  })
  prov_df <- do.call(rbind, prov_rows)

  list(
    county       = county,
    cid          = paste0("ag-", idx),
    b0_30        = sum(prov_df$b0_30),
    b31_60       = sum(prov_df$b31_60),
    b61_90       = sum(prov_df$b61_90),
    b90p         = sum(prov_df$b90p),
    v0_30        = sum(prov_df$v0_30),
    v31_60       = sum(prov_df$v31_60),
    v61_90       = sum(prov_df$v61_90),
    v90p         = sum(prov_df$v90p),
    total_claims = sum(prov_df$b0_30, prov_df$b31_60, prov_df$b61_90, prov_df$b90p),
    total_value  = sum(prov_df$v0_30, prov_df$v31_60, prov_df$v61_90, prov_df$v90p),
    providers    = prov_df
  )
}

ageing_data <- lapply(seq_along(.AG_COUNTIES), function(i)
  .ag_build_county(.AG_COUNTIES[i], i))

ageing_summary <- list(
  b0_30  = sum(sapply(ageing_data, `[[`, "b0_30")),
  b31_60 = sum(sapply(ageing_data, `[[`, "b31_60")),
  b61_90 = sum(sapply(ageing_data, `[[`, "b61_90")),
  b90p   = sum(sapply(ageing_data, `[[`, "b90p")),
  v0_30  = sum(sapply(ageing_data, `[[`, "v0_30")),
  v31_60 = sum(sapply(ageing_data, `[[`, "v31_60")),
  v61_90 = sum(sapply(ageing_data, `[[`, "v61_90")),
  v90p   = sum(sapply(ageing_data, `[[`, "v90p"))
)

# ---- Chart.js head content ---------------------------------------------------

ageing_chart_js <- paste0(
  "var ageingLabels=", jsonlite::toJSON(.AG_BUCKET_LABELS), ";",
  "var ageingColors=", jsonlite::toJSON(.AG_BUCKET_COLORS), ";",
  "var ageingCounts=", jsonlite::toJSON(c(
    ageing_summary$b0_30, ageing_summary$b31_60,
    ageing_summary$b61_90, ageing_summary$b90p
  )), ";",
  "var ageingValues=", jsonlite::toJSON(c(
    ageing_summary$v0_30, ageing_summary$v31_60,
    ageing_summary$v61_90, ageing_summary$v90p
  )), ";",
  "
  var _ageingChartsReady = false;

  function initAgeingCharts() {
    if (_ageingChartsReady || typeof Chart === 'undefined') return;

    var totalClaims = ageingCounts.reduce(function(a, b) { return a + b; }, 0);
    var totalValue  = ageingValues.reduce(function(a, b) { return a + b; }, 0);
    var barBase = { borderRadius: 4, borderSkipped: false };

    var ctx1 = document.getElementById('ageingCountChart');
    if (ctx1) {
      new Chart(ctx1, {
        type: 'bar',
        data: {
          labels: ageingLabels,
          datasets: [Object.assign({}, barBase, {
            label: 'Claims',
            data:  ageingCounts,
            backgroundColor: ageingColors
          })]
        },
        options: {
          responsive: true, maintainAspectRatio: false,
          plugins: {
            legend: { display: false },
            tooltip: {
              callbacks: {
                label: function(c) {
                  var pct = (c.parsed.y / totalClaims * 100).toFixed(1);
                  return ' ' + c.parsed.y.toLocaleString() + ' claims (' + pct + '%)';
                }
              }
            },
            datalabels: { display: false }
          },
          scales: {
            y: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b' } },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    var ctx2 = document.getElementById('ageingValueChart');
    if (ctx2) {
      new Chart(ctx2, {
        type: 'bar',
        data: {
          labels: ageingLabels,
          datasets: [Object.assign({}, barBase, {
            label: 'Value (KES)',
            data:  ageingValues,
            backgroundColor: ageingColors
          })]
        },
        options: {
          responsive: true, maintainAspectRatio: false,
          plugins: {
            legend: { display: false },
            tooltip: {
              callbacks: {
                label: function(c) {
                  var pct = (c.parsed.y / totalValue * 100).toFixed(1);
                  var m   = (c.parsed.y / 1e6).toFixed(1);
                  return ' KES ' + m + 'M (' + pct + '%)';
                }
              }
            }
          },
          scales: {
            y: {
              beginAtZero: true, grid: { color: '#f1f5f9' },
              ticks: {
                color: '#64748b',
                callback: function(v) { return 'KES ' + (v / 1e6).toFixed(0) + 'M'; }
              }
            },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    _ageingChartsReady = true;
  }

  window.revealAgeingCharts = initAgeingCharts;
  "
)

# ---- Component builders ------------------------------------------------------

.ag_bucket_card <- function(label, value_str, claim_count, color) {
  div(class = "col",
    div(class = "card border-0 shadow-sm h-100",
      style = paste0("border-top: 4px solid ", color, " !important;"),
      `data-bs-toggle`    = "tooltip",
      `data-bs-placement` = "bottom",
      title = paste0(fmt_num(claim_count), " claims"),
      div(class = "card-body px-4 py-3",
        div(class = "fw-bold lh-sm",
            style = paste0("font-size:1.55rem; color:", color, ";"),
            value_str),
        div(class = "text-uppercase fw-semibold text-muted mt-1",
            style = "font-size:.7rem; letter-spacing:.07em;", label)
      )
    )
  )
}

.ag_bar_chart_card <- function(title, canvas_id) {
  div(class = "card border-0 shadow-sm",
    div(class = "card-header bg-white border-bottom px-4 py-3",
      div(class = "fw-semibold", style = "color:#0f172a;", title)
    ),
    div(class = "card-body px-4 py-3",
      div(style = "position:relative; height:280px;",
        tags$canvas(id = canvas_id)
      )
    )
  )
}

.ag_bucket_col_th <- function(label, color) {
  tags$th(
    class = "sortable-col",
    style = paste0("color:", color, " !important; min-width:90px;"),
    label,
    tags$i(class = "bi bi-chevron-expand ms-1 sort-icon",
           style = "font-size:.7rem; color:#94a3b8;")
  )
}

.ag_county_table_rows <- function(county_list) {
  lapply(seq_along(county_list), function(i) {
    d <- county_list[[i]]
    county_row <- tags$tr(
      class = paste("county-row", if (i %% 2 == 0) "county-row-alt" else ""),
      `data-county-id` = d$cid,
      tags$td(class = "fw-semibold county-name-cell",
        tags$i(class = "bi bi-chevron-right expand-chevron me-2",
               style = "font-size:.8rem; color:#94a3b8;"),
        d$county),
      tags$td(fmt_num(d$total_claims)),
      tags$td(fmt_currency(d$total_value)),
      tags$td(fmt_num(d$b0_30)),
      tags$td(fmt_num(d$b31_60)),
      tags$td(fmt_num(d$b61_90)),
      tags$td(fmt_num(d$b90p))
    )
    prov_rows <- lapply(seq_len(nrow(d$providers)), function(j) {
      p <- d$providers[j, ]
      tags$tr(
        class = paste("provider-row", d$cid), style = "display:none;",
        tags$td(class = "provider-name-cell",
          tags$i(class = "bi bi-building me-2",
                 style = "font-size:.8rem; color:#94a3b8;"),
          tags$span(class = "text-secondary", style = "font-size:.9rem;", p$name)),
        tags$td(class = "text-secondary",
                fmt_num(p$b0_30 + p$b31_60 + p$b61_90 + p$b90p)),
        tags$td(class = "text-secondary",
                fmt_currency(p$v0_30 + p$v31_60 + p$v61_90 + p$v90p)),
        tags$td(class = "text-secondary", fmt_num(p$b0_30)),
        tags$td(class = "text-secondary", fmt_num(p$b31_60)),
        tags$td(class = "text-secondary", fmt_num(p$b61_90)),
        tags$td(class = "text-secondary", fmt_num(p$b90p))
      )
    })
    tagList(county_row, do.call(tagList, prov_rows))
  })
}

.build_ageing_table <- function(county_list) {
  if (length(county_list) == 0) {
    return(div(class = "text-center text-muted p-5",
      tags$i(class = "bi bi-funnel display-6 d-block mb-2 opacity-50"),
      "No counties match the current filters."
    ))
  }
  bucket_ths <- mapply(
    .ag_bucket_col_th, .AG_BUCKET_LABELS, .AG_BUCKET_COLORS,
    SIMPLIFY = FALSE
  )
  div(class = "table-responsive",
    tags$table(
      class = "table table-hover align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("County / Provider", "200px"),
          col_th("Total Claims"),
          col_th("Total Value (KES)", "150px"),
          do.call(tagList, bucket_ths)
        )
      ),
      tags$tbody(do.call(tagList, .ag_county_table_rows(county_list)))
    )
  )
}

# ---- Public: panel UI --------------------------------------------------------

.ageing_filter_bar <- function() {
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
            selectInput("ageing_created_month", NULL,
              choices  = c("All Months" = "",
                           setNames(.ag_months, .ag_months)),
              selected = "", width = "100%")
          )
        ),

        # Incurred Date
        div(class = "col-12 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-calendar-check"), "Incurred Date"),
            selectInput("ageing_incurred_month", NULL,
              choices  = c("All Months" = "",
                           setNames(.ag_months, .ag_months)),
              selected = "", width = "100%")
          )
        ),

        # County
        div(class = "col-12 col-md-3 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-geo-alt"), "County"),
            selectInput("ageing_county", NULL,
              choices  = c("All Counties" = "",
                           setNames(.AG_COUNTIES, .AG_COUNTIES)),
              multiple = TRUE, selectize = TRUE, width = "100%")
          )
        ),

        # Level
        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-hospital"), "Level"),
            selectInput("ageing_level", NULL,
              choices = c("All Levels" = "",
                          setNames(.AG_LEVELS, .AG_LEVELS)),
              width = "100%")
          )
        ),

        # Facility
        div(class = "col-6 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label",
              tags$i(class = "bi bi-building"), "Facility"),
            selectInput("ageing_facility", NULL,
              choices = c("All Facilities" = ""), width = "100%")
          )
        ),

        # Buttons
        div(class = "col-12 col-xl-2",
          div(class = "filter-bar-label", style = "visibility:hidden;", HTML("&nbsp;")),
          div(class = "d-flex gap-2 justify-content-end",
            actionButton("ageing_reset",
              label = tagList(tags$i(class = "bi bi-arrow-counterclockwise"), " Reset"),
              class = "btn btn-filter-reset", title = "Reset filters"),
            actionButton("ageing_apply",
              label = tagList(tags$i(class = "bi bi-funnel-fill me-1"), "Apply Filters"),
              class = "btn btn-primary btn-filter-apply")
          )
        )
      )
    )
  )
}

ageing_panel_ui <- function() {
  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "Claims Ageing Report",
      last_updated = "13 May 2026",
      source       = "Payer System + ERP",
      info         = paste0(
        "Live claims that have not been terminated or confirmed as paid. ",
        "Ageing is calculated from claim created date to today. ",
        "Excludes rejected, declined, cancelled, sent-back, missing-document, ",
        "and ERP-confirmed-paid claims."
      ),
      badges = tagList(
        tags$span(class = "badge text-bg-primary px-3 py-2 rounded-pill",   "sha_user"),
        tags$span(class = "badge text-bg-secondary px-3 py-2 rounded-pill", "dha_user")
      )
    ),

    insight_banner(
      paste0(
        fmt_num(ageing_summary$b90p), " claims aged 90+ days totalling ",
        fmt_currency(ageing_summary$v90p), " - immediate escalation required."
      ),
      sub = paste0(
        "A further ", fmt_num(ageing_summary$b61_90), " claims (",
        fmt_currency(ageing_summary$v61_90),
        ") are in the 61-90 day window."
      ),
      type = "danger"
    ),

    .ageing_filter_bar(),

    # Section 1 - Ageing bucket cards
    tags$h6(class = "ind-section-label", "Ageing Buckets"),
    div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-4 g-3",
      .ag_bucket_card("0-30 days",  fmt_currency(ageing_summary$v0_30),
                      ageing_summary$b0_30,  "#22c55e"),
      .ag_bucket_card("31-60 days", fmt_currency(ageing_summary$v31_60),
                      ageing_summary$b31_60, "#f59e0b"),
      .ag_bucket_card("61-90 days", fmt_currency(ageing_summary$v61_90),
                      ageing_summary$b61_90, "#f97316"),
      .ag_bucket_card("90+ days",        fmt_currency(ageing_summary$v90p),
                      ageing_summary$b90p,   "#ef4444")
    ),

    tags$hr(class = "my-4 border-light"),

    # Section 2 - Bar charts
    tags$h6(class = "ind-section-label", "Distribution by Ageing Bucket"),
    div(class = "row g-4",
      div(class = "col-12 col-xl-6",
        .ag_bar_chart_card("Number of Claims by Ageing Bucket",    "ageingCountChart")),
      div(class = "col-12 col-xl-6",
        .ag_bar_chart_card("Total Value (KES) by Ageing Bucket",   "ageingValueChart"))
    ),

    tags$hr(class = "my-4 border-light"),

    # Section 3 - County ageing table
    tags$h6(class = "ind-section-label", "County Breakdown"),
    div(class = "card border-0 shadow-sm",
      div(class = "card-header bg-white border-bottom d-flex justify-content-between align-items-center py-3 px-4",
        div(class = "fw-semibold", style = "color:#0f172a;", "Claims Ageing by County"),
        div(class = "d-flex align-items-center gap-2",
          uiOutput("ageing_row_count"),
          tags$button(type = "button",
            class = "btn btn-sm btn-outline-secondary",
            onclick = "showMockAction('Preparing ageing report Excel export - the file will download shortly.')",
            tags$i(class = "bi bi-file-earmark-excel me-1"), "Export"
          )
        )
      ),
      uiOutput("ageing_table_ui"),
      div(class = "card-footer bg-white border-top d-flex justify-content-between align-items-center py-2 px-4",
        uiOutput("ageing_showing"),
        uiOutput("ageing_pagination")
      )
    )
  )
}

# ---- Public: server ----------------------------------------------------------

ageing_server <- function(input, output, session) {
  PAGE_SIZE   <- 10L
  ageing_page <- reactiveVal(1L)

  sorted_data <- ageing_data[
    order(sapply(ageing_data, `[[`, "total_claims"), decreasing = TRUE)
  ]

  observeEvent(input$ageing_page_num, {
    ageing_page(as.integer(input$ageing_page_num))
  })

  total_pages <- reactive({
    max(1L, ceiling(length(sorted_data) / PAGE_SIZE))
  })

  paged_data <- reactive({
    p   <- min(ageing_page(), total_pages())
    idx <- seq.int((p - 1L) * PAGE_SIZE + 1L, min(p * PAGE_SIZE, length(sorted_data)))
    sorted_data[idx]
  })

  output$ageing_table_ui <- renderUI({ .build_ageing_table(paged_data()) })

  output$ageing_row_count <- renderUI({
    n <- length(sorted_data)
    p <- min(ageing_page(), total_pages())
    s <- (p - 1L) * PAGE_SIZE + 1L
    e <- min(p * PAGE_SIZE, n)
    tags$span(class = "badge text-bg-light border small",
              sprintf("%d-%d of %d counties", s, e, n))
  })

  output$ageing_showing <- renderUI({
    tags$span(class = "text-muted small",
      tags$i(class = "bi bi-info-circle me-1"),
      sprintf("%d counties - sorted by total claims", length(sorted_data))
    )
  })

  output$ageing_pagination <- renderUI({
    make_pagination(min(ageing_page(), total_pages()), total_pages(), "ageing_page_num")
  })

  observeEvent(input$ageing_reset, {
    updateSelectInput(session, "ageing_created_month",  selected = "")
    updateSelectInput(session, "ageing_incurred_month", selected = "")
    updateSelectInput(session, "ageing_county",         selected = character(0))
    updateSelectInput(session, "ageing_level",          selected = "")
    updateSelectInput(session, "ageing_facility",       selected = "")
    ageing_page(1L)
  })
}
