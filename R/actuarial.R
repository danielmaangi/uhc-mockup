# ==============================================================================
# Indicator: Actuarial — SHA Plus Membership & Claims Analysis
# Spec: requirements/actuarial_sha_plus.md
# Source: requirements/other/Revamp Indicators V2.xlsx
#   ("Schedule 1 - Informal/Formal Sector", "Monthly Contributions vs Claims")
# ==============================================================================

# ---- Data: Schedule 1 — active membership & claims by age band, 1 Jan-31 Dec 2025
# Real figures transcribed from the source workbook (not synthesized) — this is
# an actuarial analysis snapshot, not an operational claims-flow mockup, so the
# reference data itself is the thing being communicated to developers.

.ACT_AGE_BANDS <- c("18-25","26-30","31-35","36-40","41-45","46-50","51-55",
                    "56-60","61-65",">65")

act_schedule1 <- list(
  Informal = data.frame(
    age_band       = .ACT_AGE_BANDS,
    contributors   = c(209956,217834,192509,185998,119685,109443,99515,75599,105155,291785),
    contributions  = c(488518456,575164141,538729235,534901506,370612001,332697061,
                        309773278,270144223,381878746,1036112620),
    dependants     = c(228373,375484,468285,545449,368863,300226,217684,121322,131038,225670),
    claims         = c(252818,279712,275142,296724,205839,202067,188896,156578,220677,594165),
    amount_claimed = c(5160869578,5820408903,5867595542,6240930268,4454120059,4257341057,
                        3956385000,3221879268,4572010443,13553200542),
    stringsAsFactors = FALSE
  ),
  Formal = data.frame(
    age_band       = .ACT_AGE_BANDS,
    contributors   = c(234613,451987,470757,413627,293716,232917,182195,120617,29639,39935),
    contributions  = c(1256496023,4816255607,7589441249,8218378495,6725186934,5539621740,
                        4855813359,3598616464,557144386,539426633),
    dependants     = c(111216,492253,872339,1008456,801602,602075,404582,181391,35031,28949),
    claims         = c(75566,307258,457762,450456,367614,322611,276157,226334,27934,35041),
    amount_claimed = c(990349640,3400779234,4632218136,4317014288,3185574783,2693244799,
                        2291568960,1761189014,456629741,764089092),
    stringsAsFactors = FALSE
  )
)

# Contributors/dependants/claims whose DOB is blank in the source cannot be
# age-banded — excluded from the bands and from the TOTAL row, shown as a
# separate footnote line rather than dropped, per the workbook's own note.
act_age_not_captured <- list(
  Informal = list(contributors = 624555, contributions = 1239706566,
                   dependants = 543772, claims = 626407, amount_claimed = 16752118444),
  Formal   = list(contributors = 1299297, contributions = 29328871289,
                   dependants = 509824, claims = 253468, amount_claimed = 4651275863)
)

.act_totals <- function(df) list(
  contributors   = sum(df$contributors),
  contributions  = sum(df$contributions),
  dependants     = sum(df$dependants),
  claims         = sum(df$claims),
  amount_claimed = sum(df$amount_claimed)
)

act_sector_totals <- list(
  Informal = .act_totals(act_schedule1$Informal),
  Formal   = .act_totals(act_schedule1$Formal)
)
act_sector_totals$Overall <- list(
  contributors   = act_sector_totals$Informal$contributors   + act_sector_totals$Formal$contributors,
  contributions  = act_sector_totals$Informal$contributions  + act_sector_totals$Formal$contributions,
  dependants     = act_sector_totals$Informal$dependants     + act_sector_totals$Formal$dependants,
  claims         = act_sector_totals$Informal$claims         + act_sector_totals$Formal$claims,
  amount_claimed = act_sector_totals$Informal$amount_claimed + act_sector_totals$Formal$amount_claimed
)

# Loss ratio (claims / contributions) — not a workbook column, but directly
# derivable from the two totals already reported side by side, and the single
# most important actuarial read of this schedule.
for (sec in names(act_sector_totals)) {
  t <- act_sector_totals[[sec]]
  act_sector_totals[[sec]]$loss_ratio <- round(t$amount_claimed / t$contributions, 2)
  act_sector_totals[[sec]]$avg_claim_per_contributor <- round(t$amount_claimed / t$contributors)
}

.ACT_SECTORS <- c("Informal", "Formal", "Overall")

# ---- Data: Monthly Contributions vs Claims, Oct 2024 - Jun 2026 (to date) -------
# Claims counted by admission_date; amount = SHIF FUND + ECCIF FUND; files
# joined on cr_id. No. of CRs is a distinct-contributors-in-month count and is
# not additive across months, so it is not summed to a total (per source note).

act_monthly <- data.frame(
  month = c("2024-10","2024-11","2024-12","2025-01","2025-02","2025-03","2025-04",
            "2025-05","2025-06","2025-07","2025-08","2025-09","2025-10","2025-11",
            "2025-12","2026-01","2026-02","2026-03","2026-04","2026-05","2026-06"),
  formal_crs           = c(2746777,2924325,2925522,3047759,3076380,3104015,3113681,
                            3146884,3171572,3184447,3149868,3207158,3249472,3270397,
                            3214516,3235675,3258550,3271165,3284520,3204252,131840),
  formal_contributions = c(5503140587,5729769639,5821805861,5928398062,5882076296,
                            5875748588,5918081885,5959269667,6014988655,6006022801,
                            5815623921,6114916277,6162363359,7072168044,6342576050,
                            6337074872,6320431474,6353949128,6392897836,6920927987,93987571),
  formal_claims        = c(23966,70544,85638,113846,151693,205161,230368,271755,281884,
                            269512,217485,192911,194533,197301,478965,640549,647465,
                            676909,745603,1019461,658580),
  formal_amount        = c(601482992,1402694891,1519953470,2151468172,2365535274,
                            2632381198,3068898525,3245673784,2863488123,2516632481,
                            2285375981,1992625854,1999849563,2117237641,1950594090,
                            1985064381,2003015645,2221333318,2216528546,2056541433,1161083586),
  informal_crs           = c(21992,364232,463117,610020,785765,900939,968038,1021381,
                              891693,931248,985010,1053212,1095596,1113366,1121100,
                              1137928,1133935,1147493,1153822,1131338,1003843),
  informal_contributions = c(751611,180871623,230320024,329063448,416604288,463233241,
                              499645421,528943275,449629284,470440543,499161536,536702169,
                              562308223,574328727,582322709,592548979,589638800,597666656,
                              601818879,591708471,534179378),
  informal_claims        = c(53635,171274,206339,256688,283294,316099,348269,379416,
                              300859,270285,246705,234659,231736,225629,224998,237716,
                              232011,259158,253654,266706,163790),
  informal_amount        = c(1610084314,4037577467,4293366874,5806828936,6091005210,
                              6611791074,7521717051,8089639871,6394966794,6079567007,
                              5609014798,5555144874,5580472721,5546521526,5316911661,
                              5521303102,5533294075,6030272042,5985688560,5921771944,3313260898),
  stringsAsFactors = FALSE
)
act_monthly$month_label <- format(as.Date(paste0(act_monthly$month, "-01")), "%b '%y")

act_monthly_totals <- list(
  formal_contributions   = sum(act_monthly$formal_contributions),
  formal_claims          = sum(act_monthly$formal_claims),
  formal_amount          = sum(act_monthly$formal_amount),
  informal_contributions = sum(act_monthly$informal_contributions),
  informal_claims        = sum(act_monthly$informal_claims),
  informal_amount        = sum(act_monthly$informal_amount)
)
act_monthly_totals$formal_loss_ratio   <- round(act_monthly_totals$formal_amount   / act_monthly_totals$formal_contributions, 2)
act_monthly_totals$informal_loss_ratio <- round(act_monthly_totals$informal_amount / act_monthly_totals$informal_contributions, 2)

# ---- Chart.js head content --------------------------------------------------------

act_chart_js <- paste0(
  "var actAgeBands=", jsonlite::toJSON(.ACT_AGE_BANDS), ";",
  "var actAgeContrib=", jsonlite::toJSON(list(
      Informal = act_schedule1$Informal$contributions,
      Formal   = act_schedule1$Formal$contributions
    )), ";",
  "var actAgeClaims=", jsonlite::toJSON(list(
      Informal = act_schedule1$Informal$amount_claimed,
      Formal   = act_schedule1$Formal$amount_claimed
    )), ";",
  "var actMonthLabels=", jsonlite::toJSON(act_monthly$month_label), ";",
  "var actMonthlyFormalContrib=",   jsonlite::toJSON(act_monthly$formal_contributions), ";",
  "var actMonthlyFormalClaims=",    jsonlite::toJSON(act_monthly$formal_amount), ";",
  "var actMonthlyInformalContrib=", jsonlite::toJSON(act_monthly$informal_contributions), ";",
  "var actMonthlyInformalClaims=",  jsonlite::toJSON(act_monthly$informal_amount), ";",
  "
  var _actAgeCharts = {};
  var _actMonthlyChart = null;

  function initActAgeChart(sector) {
    if (_actAgeCharts[sector] || typeof Chart === 'undefined') return;
    var ctx = document.getElementById('actAgeChart-' + sector);
    if (!ctx) return;
    var contrib = actAgeContrib[sector] || actAgeContrib['Informal'];
    var claims  = actAgeClaims[sector]  || actAgeClaims['Informal'];
    _actAgeCharts[sector] = new Chart(ctx, {
      type: 'bar',
      data: {
        labels: actAgeBands,
        datasets: [
          { label: 'Contributions', data: contrib,
            backgroundColor: 'rgba(2,132,199,0.7)', borderColor: '#0284c7', borderWidth: 1, borderRadius: 4 },
          { label: 'Amount Claimed', data: claims,
            backgroundColor: 'rgba(220,38,38,0.7)', borderColor: '#dc2626', borderWidth: 1, borderRadius: 4 }
        ]
      },
      options: {
        responsive: true, maintainAspectRatio: false,
        plugins: {
          legend: { position: 'top', labels: { usePointStyle: true, pointStyleWidth: 10, boxHeight: 6 } },
          tooltip: { callbacks: { label: function(c) {
            return ' ' + c.dataset.label + ': KES ' + (c.parsed.y / 1e6).toFixed(1) + 'M';
          } } }
        },
        scales: {
          y: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b',
                 callback: function(v) { return (v/1e6).toFixed(0) + 'M'; } } },
          x: { title: { display: true, text: 'Age band', color: '#64748b' },
               grid: { display: false }, ticks: { color: '#64748b' } }
        }
      }
    });
  }

  function initActMonthlyChart() {
    if (_actMonthlyChart || typeof Chart === 'undefined') return;
    var ctx = document.getElementById('actMonthlyChart');
    if (!ctx) return;
    _actMonthlyChart = new Chart(ctx, {
      data: {
        labels: actMonthLabels,
        datasets: [
          { type: 'line', label: 'Formal Contributions', data: actMonthlyFormalContrib,
            borderColor: '#0284c7', backgroundColor: '#0284c7', tension: 0.3, pointRadius: 2, borderWidth: 2 },
          { type: 'line', label: 'Formal Claims', data: actMonthlyFormalClaims,
            borderColor: '#7dd3fc', backgroundColor: '#7dd3fc', tension: 0.3, pointRadius: 2, borderWidth: 2, borderDash: [4,3] },
          { type: 'line', label: 'Informal Contributions', data: actMonthlyInformalContrib,
            borderColor: '#16a34a', backgroundColor: '#16a34a', tension: 0.3, pointRadius: 2, borderWidth: 2 },
          { type: 'line', label: 'Informal Claims', data: actMonthlyInformalClaims,
            borderColor: '#dc2626', backgroundColor: '#dc2626', tension: 0.3, pointRadius: 2, borderWidth: 2, borderDash: [4,3] }
        ]
      },
      options: {
        responsive: true, maintainAspectRatio: false,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: { position: 'top', labels: { usePointStyle: true, pointStyleWidth: 10, boxHeight: 6 } },
          tooltip: { callbacks: { label: function(c) {
            return ' ' + c.dataset.label + ': KES ' + (c.parsed.y / 1e6).toFixed(1) + 'M';
          } } }
        },
        scales: {
          y: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b',
                 callback: function(v) { return (v/1e6).toFixed(0) + 'M'; } } },
          x: { grid: { display: false }, ticks: { color: '#64748b', maxRotation: 45, minRotation: 45, font: { size: 9 } } }
        }
      }
    });
  }

  document.addEventListener('shown.bs.tab', function(e) {
    var target = e.target.getAttribute('data-bs-target') || '';
    if (!target.startsWith('#act-pane-')) return;
    setTimeout(function() { initActAgeChart(target.replace('#act-pane-', '')); }, 50);
  });

  window.revealActCharts = function() {
    initActAgeChart('Informal');
    initActMonthlyChart();
  };
  "
)

# ---- Component builders -----------------------------------------------------------

.act_metric_card <- function(label, value, icon_cls, color, sub_value = NULL) {
  div(class = "col",
    div(class = "card border-0 shadow-sm h-100",
      div(class = "card-body d-flex align-items-center gap-3 p-4",
        div(class = "flex-shrink-0 rounded-3 d-flex align-items-center justify-content-center",
          style = paste0("width:52px; height:52px; background:", color, "18;"),
          tags$i(class = icon_cls, style = paste0("font-size:1.4rem; color:", color, ";"))
        ),
        div(class = "flex-grow-1 min-width-0",
          div(class = "text-uppercase fw-semibold text-muted mb-1 text-truncate",
              style = "font-size:.7rem; letter-spacing:.07em;", label),
          div(class = "fw-bold lh-sm", style = "font-size:1.5rem; color:#0f172a;", value),
          if (!is.null(sub_value)) div(class = "text-muted mt-1", style = "font-size:.85rem;", sub_value)
        )
      )
    )
  )
}

.act_metrics_row <- function(t) {
  div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-4 g-3",
    .act_metric_card("Contributors", fmt_num(t$contributors), "bi bi-person-check-fill", "#0284c7"),
    .act_metric_card("Contributions", fmt_currency(t$contributions), "bi bi-cash-stack", "#16a34a"),
    .act_metric_card("Dependants", fmt_num(t$dependants), "bi bi-people-fill", "#8b5cf6"),
    .act_metric_card("Claims", fmt_num(t$claims), "bi bi-file-earmark-medical-fill", "#f59e0b"),
    .act_metric_card("Amount Claimed", fmt_currency(t$amount_claimed), "bi bi-receipt-cutoff", "#ea580c"),
    .act_metric_card("Avg Claim / Contributor", fmt_currency(t$avg_claim_per_contributor),
                     "bi bi-calculator-fill", "#0ea5e9"),
    .act_metric_card("Loss Ratio", paste0(t$loss_ratio, "x"),
                     "bi bi-graph-up-arrow", if (t$loss_ratio > 1) "#dc2626" else "#16a34a",
                     sub_value = "Amount claimed ÷ contributions")
  )
}

.act_age_table_ui <- function(df, not_captured) {
  rows <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    avg <- round(r$amount_claimed / r$contributors)
    tags$tr(
      tags$td(class = "fw-medium", r$age_band),
      tags$td(fmt_num(r$contributors)),
      tags$td(fmt_currency(r$contributions)),
      tags$td(fmt_num(r$dependants)),
      tags$td(fmt_num(r$claims)),
      tags$td(fmt_currency(r$amount_claimed)),
      tags$td(fmt_num(avg))
    )
  })
  totals <- .act_totals(df)
  total_row <- tags$tr(class = "table-secondary fw-semibold",
    tags$td("TOTAL"),
    tags$td(fmt_num(totals$contributors)),
    tags$td(fmt_currency(totals$contributions)),
    tags$td(fmt_num(totals$dependants)),
    tags$td(fmt_num(totals$claims)),
    tags$td(fmt_currency(totals$amount_claimed)),
    tags$td(fmt_num(round(totals$amount_claimed / totals$contributors)))
  )
  nc_row <- tags$tr(class = "text-muted", style = "font-size:.85rem;",
    tags$td(tags$em("Age not captured")),
    tags$td(fmt_num(not_captured$contributors)),
    tags$td(fmt_currency(not_captured$contributions)),
    tags$td(fmt_num(not_captured$dependants)),
    tags$td(fmt_num(not_captured$claims)),
    tags$td(fmt_currency(not_captured$amount_claimed)),
    tags$td("—")
  )
  div(class = "table-responsive",
    tags$table(class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("Age Band", "110px"), col_th("Contributors"), col_th("Contributions"),
          col_th("Dependants"), col_th("Claims"), col_th("Amount Claimed"),
          col_th("Avg Claim / Contributor")
        )
      ),
      tags$tbody(do.call(tagList, rows), total_row, nc_row)
    ),
    div(class = "px-3 py-2 text-muted", style = "font-size:.78rem;",
      "Age not captured: contributors with a blank date of birth in the source; excluded from the age bands and TOTAL row above.")
  )
}

.act_tab_pane <- function(sector, is_active = FALSE) {
  t <- act_sector_totals[[sector]]
  div(id = paste0("act-pane-", sector),
    class = paste("tab-pane fade", if (is_active) "show active" else ""),
    role = "tabpanel",
    div(class = "pt-3",
      tags$h6(class = "ind-section-label", "Summary Metrics"),
      .act_metrics_row(t),

      tags$hr(class = "my-4 border-light"),

      tags$h6(class = "ind-section-label", "Contributions vs Claims by Age Band"),
      div(class = "card border-0 shadow-sm mb-4",
        div(class = "card-body px-4 py-3",
          div(style = "position:relative; height:300px;",
              tags$canvas(id = paste0("actAgeChart-", sector)))
        )
      ),

      if (sector != "Overall") tagList(
        tags$h6(class = "ind-section-label", "Schedule 1 — Age-Band Membership & Claims"),
        div(class = "card border-0 shadow-sm",
          .act_age_table_ui(act_schedule1[[sector]], act_age_not_captured[[sector]]))
      ) else div(class = "text-muted", style = "font-size:.85rem;",
        "Combined Informal + Formal totals shown above; see the Informal and Formal tabs for the full age-band schedule.")
    )
  )
}

# ---- Monthly trend table ------------------------------------------------------------

.act_monthly_table_ui <- function() {
  rows <- lapply(seq_len(nrow(act_monthly)), function(i) {
    r <- act_monthly[i, ]
    tags$tr(
      tags$td(class = "fw-medium", r$month_label),
      tags$td(fmt_num(r$formal_crs)),
      tags$td(fmt_currency(r$formal_contributions)),
      tags$td(fmt_num(r$formal_claims)),
      tags$td(fmt_currency(r$formal_amount)),
      tags$td(fmt_num(r$informal_crs)),
      tags$td(fmt_currency(r$informal_contributions)),
      tags$td(fmt_num(r$informal_claims)),
      tags$td(fmt_currency(r$informal_amount))
    )
  })
  total_row <- tags$tr(class = "table-secondary fw-semibold",
    tags$td("TOTAL"), tags$td("—"),
    tags$td(fmt_currency(act_monthly_totals$formal_contributions)),
    tags$td(fmt_num(act_monthly_totals$formal_claims)),
    tags$td(fmt_currency(act_monthly_totals$formal_amount)),
    tags$td("—"),
    tags$td(fmt_currency(act_monthly_totals$informal_contributions)),
    tags$td(fmt_num(act_monthly_totals$informal_claims)),
    tags$td(fmt_currency(act_monthly_totals$informal_amount))
  )
  div(class = "table-responsive", style = "max-height:480px; overflow-y:auto;",
    tags$table(class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("Month", "90px"),
          col_th("Formal CRs"), col_th("Formal Contributions"), col_th("Formal Claims"), col_th("Formal Amount"),
          col_th("Informal CRs"), col_th("Informal Contributions"), col_th("Informal Claims"), col_th("Informal Amount")
        )
      ),
      tags$tbody(do.call(tagList, rows), total_row)
    ),
    div(class = "px-3 py-2 text-muted", style = "font-size:.78rem;",
      "No. of CRs = distinct contributors who contributed that month — not additive across months, so no column total is shown for it.")
  )
}

# ---- Public: panel UI --------------------------------------------------------------

act_panel_ui <- function() {
  ov <- act_sector_totals$Overall
  inf <- act_sector_totals$Informal
  frm <- act_sector_totals$Formal

  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "SHA Plus — Membership & Claims Analysis",
      last_updated = "10 Aug 2026",
      source        = "member_contribution_combined.csv × household_dependent_combined.csv × util.csv, joined on cr_id",
      info = paste0(
        "Aggregated active membership and claims data, Informal vs Formal sector, ",
        "1 Jan-31 Dec 2025 (Schedule 1) and Oct 2024-Jun 2026 (monthly trend). Amount ",
        "claimed = SHIF FUND + ECCIF FUND. Sector is taken from member_type; age band ",
        "is contributor age as at 31 Dec 2025. Contributors with a blank date of birth ",
        "cannot be age-banded and are excluded from the bands and totals."
      ),
      badges = tagList(
        tags$span(class = "badge text-bg-primary px-3 py-2 rounded-pill",   "Actuarial"),
        tags$span(class = "badge text-bg-secondary px-3 py-2 rounded-pill", "DHA Leadership")
      )
    ),

    insight_banner(
      paste0(
        "Informal-sector claims run ", inf$loss_ratio, "x contributions (",
        fmt_currency(inf$amount_claimed), " claimed vs ", fmt_currency(inf$contributions),
        " contributed) for 2025, compared with ", frm$loss_ratio,
        "x for the Formal sector — the Formal sector is cross-subsidising Informal-sector claims."
      ),
      sub = paste0("Combined loss ratio: ", ov$loss_ratio, "x."),
      type = "warning"
    ),

    tags$ul(class = "nav nav-pills nav-fill mb-3", role = "tablist",
      lapply(.ACT_SECTORS, function(s) {
        tags$li(class = "nav-item", role = "presentation",
          tags$button(
            class = paste("nav-link", if (s == "Informal") "active" else ""),
            id = paste0("act-tab-", s),
            `data-bs-toggle` = "pill",
            `data-bs-target` = paste0("#act-pane-", s),
            type = "button", role = "tab", s
          )
        )
      })
    ),

    div(class = "tab-content",
      .act_tab_pane("Informal", is_active = TRUE),
      .act_tab_pane("Formal"),
      .act_tab_pane("Overall")
    ),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label", "Monthly Contributions vs Claims (Oct 2024 – Jun 2026)"),
    div(class = "card border-0 shadow-sm mb-4",
      div(class = "card-body px-4 py-3",
        div(style = "position:relative; height:320px;", tags$canvas(id = "actMonthlyChart"))
      )
    ),
    div(class = "card border-0 shadow-sm", .act_monthly_table_ui())
  )
}

# ---- Public: server ------------------------------------------------------------------

act_server <- function(input, output, session) {}

# ---- Data Model samples ---------------------------------------------------------------

act_raw_sample <- data.frame(
  cr_id             = c("CR-104822","CR-104823","CR-104824","CR-104825","CR-104826"),
  sector            = c("Informal","Formal","Informal","Formal","Informal"),
  age_band          = c("31-35","41-45","56-60",">65","26-30"),
  contribution_period = c("2025-06","2025-06","2025-07","2025-07","2025-08"),
  contribution_amount = c(1200,4500,1200,4500,1200),
  dependants_count  = c(2,3,1,0,4),
  claim_amount      = c(18400,32100,9800,54200,12300),
  claim_fund        = c("SHIF","ECCIF","SHIF","SHIF","ECCIF"),
  admission_date    = c("2025-06-14","2025-06-22","2025-07-03","2025-07-19","2025-08-02"),
  stringsAsFactors = FALSE
)

act_modelled_sample <- data.frame(
  sector          = c(rep("Informal", 3), rep("Formal", 2)),
  age_band        = c("18-25","26-30","31-35","18-25","26-30"),
  contributors    = c(act_schedule1$Informal$contributors[1:3], act_schedule1$Formal$contributors[1:2]),
  contributions_KES = round(c(act_schedule1$Informal$contributions[1:3], act_schedule1$Formal$contributions[1:2])),
  claims          = c(act_schedule1$Informal$claims[1:3], act_schedule1$Formal$claims[1:2]),
  amount_claimed_KES = round(c(act_schedule1$Informal$amount_claimed[1:3], act_schedule1$Formal$amount_claimed[1:2])),
  stringsAsFactors = FALSE
)
