# ==============================================================================
# Indicator: Ambulance Claims
# Spec: requirements/ambulance_claims_indicators.md
# ==============================================================================

# ---- Constants -----------------------------------------------------------

.AMB_VENDORS <- c(
  "St John Ambulance Kenya", "Kenya Red Cross Ambulance", "AMREF Flying Doctors",
  "Rescue.co Kenya", "Nairobi County EMS", "AAR Ambulance Services",
  "Avenue Healthcare Ambulance", "Uzima Ambulance Services"
)

.AMB_COUNTIES <- c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret",
                    "Thika", "Kitale", "Garissa", "Meru", "Nyeri")

# Display/card order (Count/Amount cards, trend status toggle). "submitted" is
# not an independent claim state - it is the TOTAL of the four real pipeline
# statuses below (every claim that has been filed is already at minimum
# "Under Review"). Kept first in this list purely for card ordering; Under
# Review is kept last as the "earliest, least-resolved" status.
.AMB_STATUSES <- c("submitted", "paid", "returned", "rejected", "under_review")

# The four real, mutually-exclusive states a claim can carry. Used to generate
# the mock data - order here is independent of the display order above, and
# "paid" must stay last so it absorbs the mock-data rounding remainder (see
# .make_amb_combo / .amb_split_monthly).
.AMB_PIPELINE_STATUSES <- c("under_review", "returned", "rejected", "paid")

.AMB_STATUS_LABELS <- c(
  submitted     = "Submitted",
  under_review  = "Under Review",
  returned      = "Returned",
  rejected      = "Rejected",
  paid          = "Paid"
)

.AMB_STATUS_COLORS <- c(
  submitted     = "#0f172a",
  under_review  = "#f59e0b",
  returned      = "#f97316",
  rejected      = "#dc2626",
  paid          = "#16a34a"
)

# Outstanding = still live in the pipeline (not yet resolved to paid/rejected)
.AMB_OUTSTANDING_STATUSES <- c("under_review", "returned")

.AMB_AGE_BUCKET_LABELS <- c("0-30 days", "31-60 days", "61-90 days", "90+ days")
.AMB_AGE_BUCKET_COLORS <- c("#22c55e", "#f59e0b", "#f97316", "#ef4444")

.amb_months <- format(seq(as.Date("2025-05-01"), by = "month", length.out = 12), "%b '%y")

# ---- Data: vendor x county status grid ------------------------------------
# Reconciliation identity (holds by construction):
#   Total Submitted = Paid + Rejected + Outstanding
#   Outstanding     = Under Review + Returned
#   Submitted       = Under Review + Returned + Rejected + Paid (the total)

# Status mix (must sum to 1) and a value multiplier per status - rejected
# claims skew toward higher-value inter-facility transfers, so % of value
# rejected runs higher than % of count rejected.
# NOTE: order here must match .AMB_PIPELINE_STATUSES exactly - `names(counts)
# <- .AMB_PIPELINE_STATUSES` below relies on positional alignment, and "paid"
# must stay last so it absorbs the rounding remainder.
.amb_status_probs <- c(under_review = 0.16, returned = 0.07,
                        rejected = 0.13, paid = 0.64)
.amb_status_val_mult <- c(under_review = 1.0, returned = 1.1,
                           rejected = 1.4, paid = 0.95)

.make_amb_combo <- function(vendor, county, vi, ci) {
  set.seed(vi * 131 + ci * 17)
  n <- sample(40:260, 1)

  probs  <- pmax(0.01, .amb_status_probs + runif(4, -0.03, 0.03))
  probs  <- probs / sum(probs)
  counts <- as.integer(floor(n * probs))
  counts[4] <- n - sum(counts[1:3])   # "paid" takes the remainder - exact tie-out
  names(counts) <- .AMB_PIPELINE_STATUSES

  avg_val <- runif(1, 8000, 35000)
  values  <- counts * avg_val * .amb_status_val_mult * runif(4, 0.85, 1.15)
  names(values) <- .AMB_PIPELINE_STATUSES

  data.frame(
    vendor = vendor, county = county,
    cnt_under_review = counts[["under_review"]], cnt_returned = counts[["returned"]],
    cnt_rejected     = counts[["rejected"]],      cnt_paid     = counts[["paid"]],
    val_under_review = values[["under_review"]], val_returned = values[["returned"]],
    val_rejected     = values[["rejected"]],      val_paid     = values[["paid"]],
    stringsAsFactors = FALSE
  )
}

amb_grid <- do.call(rbind, unlist(lapply(seq_along(.AMB_VENDORS), function(vi) {
  lapply(seq_along(.AMB_COUNTIES), function(ci) {
    .make_amb_combo(.AMB_VENDORS[vi], .AMB_COUNTIES[ci], vi, ci)
  })
}), recursive = FALSE))

.amb_cnt_cols <- c("cnt_under_review", "cnt_returned", "cnt_rejected", "cnt_paid")
.amb_val_cols <- c("val_under_review", "val_returned", "val_rejected", "val_paid")

.amb_rollup <- function(df, group_col) {
  agg_cnt <- aggregate(df[.amb_cnt_cols], by = list(group = df[[group_col]]), FUN = sum)
  agg_val <- aggregate(df[.amb_val_cols], by = list(group = df[[group_col]]), FUN = sum)
  out <- merge(agg_cnt, agg_val, by = "group")
  names(out)[names(out) == "group"] <- group_col

  out$total_claims <- rowSums(out[.amb_cnt_cols])   # = "Submitted" (total of the 4 statuses)
  out$total_value  <- rowSums(out[.amb_val_cols])
  out$outstanding_claims <- out$cnt_under_review + out$cnt_returned
  out$outstanding_value  <- out$val_under_review + out$val_returned
  out$rejection_rate_cnt <- round(out$cnt_rejected / out$total_claims * 100, 1)
  out$rejection_rate_val <- round(out$val_rejected / out$total_value  * 100, 1)

  set.seed(nrow(out) * 37)
  out$avg_tat <- sample(6:22, nrow(out), replace = TRUE)
  out
}

amb_by_vendor <- .amb_rollup(amb_grid, "vendor")
amb_by_county <- .amb_rollup(amb_grid, "county")

# ---- Summary (overall) ------------------------------------------------------

amb_summary <- list(
  total_claims         = sum(amb_grid[.amb_cnt_cols]),
  total_value          = sum(amb_grid[.amb_val_cols]),
  under_review_claims  = sum(amb_grid$cnt_under_review),
  under_review_value   = sum(amb_grid$val_under_review),
  returned_claims      = sum(amb_grid$cnt_returned),
  returned_value       = sum(amb_grid$val_returned),
  rejected_claims      = sum(amb_grid$cnt_rejected),
  rejected_value       = sum(amb_grid$val_rejected),
  paid_claims          = sum(amb_grid$cnt_paid),
  paid_value           = sum(amb_grid$val_paid),
  outstanding_claims   = sum(amb_grid$cnt_under_review, amb_grid$cnt_returned),
  outstanding_value    = sum(amb_grid$val_under_review, amb_grid$val_returned),
  unique_vendors       = length(.AMB_VENDORS)
)
# "Submitted" = total of the four real pipeline statuses, not an independent bucket.
amb_summary$submitted_claims <- amb_summary$total_claims
amb_summary$submitted_value  <- amb_summary$total_value
amb_summary$rejection_rate_cnt <- round(amb_summary$rejected_claims / amb_summary$total_claims * 100, 1)
amb_summary$rejection_rate_val <- round(amb_summary$rejected_value  / amb_summary$total_value  * 100, 1)

set.seed(919)
amb_summary$unique_patients <- round(amb_summary$total_claims * runif(1, 0.75, 0.90))

# TAT: submission -> payment date. Only PAID claims have a payment date;
# Submitted/Under Review/Returned/Rejected claims are excluded from the TAT
# sample since they were never (or not yet) paid. A long right tail of stuck
# claims means the mean runs meaningfully above the median.
set.seed(555)
.amb_tat_core    <- rpois(950, lambda = 6) + 1
.amb_tat_outlier <- sample(30:150, 50, replace = TRUE)
.amb_tat_sample  <- c(.amb_tat_core, .amb_tat_outlier)
amb_summary$avg_tat    <- round(mean(.amb_tat_sample), 1)
amb_summary$median_tat <- median(.amb_tat_sample)
amb_summary$min_tat    <- min(.amb_tat_sample)
amb_summary$max_tat    <- max(.amb_tat_sample)

# ---- Data: breakdown by intervention (SHA benefit-package line) -----------
# Several named interventions share one SHA code (e.g. Severe burns / Head
# injuries / Severe wounds / Multiple fractures all bill under SHA-01-004),
# so the breakdown groups by the more granular intervention *name*, with the
# SHA code carried alongside for reference - same code can repeat across rows.
# Scoped to the clinical *reason* for dispatch only - the transport line items
# (SHA-01-001/002, billed on every dispatch) and antidote treatments
# (Anti-Rabies, Anti-Snake Venom - the treatment given, not the reason for
# the call) are excluded as they don't read as a "reason for the ambulance".

.AMB_INTERVENTIONS <- data.frame(
  code = c("SHA-01-003", "SHA-01-004", "SHA-01-004", "SHA-01-004", "SHA-01-004",
           "SHA-01-005", "SHA-01-005", "SHA-01-005", "SHA-01-006", "SHA-01-006",
           "SHA-01-007", "SHA-01-008", "SHA-01-010", "SHA-01-013", "SHA-01-013"),
  name = c(
    "Cardiac/Respiratory Arrest",
    "Severe burns", "Head injuries", "Severe wounds", "Multiple fractures",
    "Haemorrhagic", "Septic", "Dehydration",
    "Unconsciousness", "Confusion",
    "Severe respiratory distress",
    "Seizures/Status epilepticus",
    "Stroke",
    "Acute Coronary Syndrome", "Pulmonary embolism"
  ),
  # Relative claim-volume weight - clinical indication codes vary with
  # acuity/prevalence (e.g. head injuries and unconsciousness are far more
  # common dispatch reasons than pulmonary embolism).
  weight = c(7, 3, 6, 5, 6, 4, 3, 5, 6, 3, 5, 4, 5, 2, 1),
  stringsAsFactors = FALSE
)

# Same per-row status split as .make_amb_combo (jittered .amb_status_probs,
# "paid" absorbs the rounding remainder) so each intervention row satisfies
# the reconciliation identity on its own, independent of the vendor/county grid.
.make_amb_intervention_row <- function(code, name, n, idx) {
  set.seed(idx * 151 + n)
  probs  <- pmax(0.01, .amb_status_probs + runif(4, -0.03, 0.03))
  probs  <- probs / sum(probs)
  counts <- as.integer(floor(n * probs))
  counts[4] <- n - sum(counts[1:3])
  names(counts) <- .AMB_PIPELINE_STATUSES

  avg_val <- runif(1, 7000, 40000)
  values  <- counts * avg_val * .amb_status_val_mult * runif(4, 0.85, 1.15)
  names(values) <- .AMB_PIPELINE_STATUSES

  data.frame(
    code = code, intervention = name,
    cnt_under_review = counts[["under_review"]], cnt_returned = counts[["returned"]],
    cnt_rejected     = counts[["rejected"]],      cnt_paid     = counts[["paid"]],
    val_under_review = values[["under_review"]], val_returned = values[["returned"]],
    val_rejected     = values[["rejected"]],      val_paid     = values[["paid"]],
    stringsAsFactors = FALSE
  )
}

.amb_intervention_n <- pmax(15, round(.AMB_INTERVENTIONS$weight / sum(.AMB_INTERVENTIONS$weight) *
                                       amb_summary$total_claims))

amb_by_intervention <- do.call(rbind, lapply(seq_len(nrow(.AMB_INTERVENTIONS)), function(i) {
  .make_amb_intervention_row(.AMB_INTERVENTIONS$code[i], .AMB_INTERVENTIONS$name[i],
                              .amb_intervention_n[i], i)
}))

amb_by_intervention$total_claims       <- rowSums(amb_by_intervention[.amb_cnt_cols])
amb_by_intervention$total_value        <- rowSums(amb_by_intervention[.amb_val_cols])
amb_by_intervention$outstanding_claims <- amb_by_intervention$cnt_under_review + amb_by_intervention$cnt_returned
amb_by_intervention$outstanding_value  <- amb_by_intervention$val_under_review + amb_by_intervention$val_returned
amb_by_intervention$rejection_rate_cnt <- round(amb_by_intervention$cnt_rejected / amb_by_intervention$total_claims * 100, 1)
amb_by_intervention$rejection_rate_val <- round(amb_by_intervention$val_rejected / amb_by_intervention$total_value  * 100, 1)

set.seed(nrow(amb_by_intervention) * 53)
amb_by_intervention$avg_tat <- sample(5:24, nrow(amb_by_intervention), replace = TRUE)

amb_by_intervention <- amb_by_intervention[order(-amb_by_intervention$total_claims), ]

# ---- Aging of pending (outstanding) claims - toggle: created vs incurred ---
# Same outstanding population, two different bucket assignments depending on
# which date drives the calculation. Incurred-date buckets skew older because
# the incident date always precedes (or equals) the claim's created date.

.amb_age_props_created  <- c(0.35, 0.32, 0.20, 0.13)
.amb_age_props_incurred <- c(0.18, 0.30, 0.28, 0.24)

.amb_age_split <- function(total_cnt, total_val, props) {
  cnt <- as.integer(floor(total_cnt * props))
  cnt[4] <- total_cnt - sum(cnt[1:3])
  val <- total_val * props
  val[4] <- total_val - sum(val[1:3])
  list(cnt = cnt, val = val)
}

amb_aging <- list(
  created  = .amb_age_split(amb_summary$outstanding_claims, amb_summary$outstanding_value,
                             .amb_age_props_created),
  incurred = .amb_age_split(amb_summary$outstanding_claims, amb_summary$outstanding_value,
                             .amb_age_props_incurred)
)

# ---- Monthly trend - toggles: created/incurred date, and status ------------
# "Submitted" trend = the total series itself (sum of the four pipeline
# statuses for that month), consistent with Submitted being a derived total
# everywhere else on this page.

set.seed(246)
.amb_trend_vol_created  <- sample(180:520, 12, replace = TRUE)
.amb_trend_val_created  <- round(.amb_trend_vol_created * runif(12, 12000, 28000))
set.seed(357)
.amb_trend_vol_incurred <- pmax(60, round(.amb_trend_vol_created * runif(12, 0.85, 1.1)))
.amb_trend_val_incurred <- round(.amb_trend_vol_incurred * runif(12, 12000, 28000))

# Split a 12-month total into per-status monthly counts/values, mirroring the
# same status mix used to build the vendor/county grid (with a value-weighted
# mix for the value split, since rejected claims skew higher-value).
.amb_status_val_probs <- .amb_status_probs * .amb_status_val_mult
.amb_status_val_probs <- .amb_status_val_probs / sum(.amb_status_val_probs)

.amb_split_monthly <- function(totals, probs, seed_offset) {
  out <- matrix(0, nrow = length(totals), ncol = length(probs),
                 dimnames = list(NULL, names(probs)))
  for (m in seq_along(totals)) {
    set.seed(seed_offset + m * 11)
    p <- pmax(0.01, probs + runif(length(probs), -0.03, 0.03))
    p <- p / sum(p)
    v <- floor(totals[m] * p)
    v[length(p)] <- totals[m] - sum(v[-length(p)])   # "paid" absorbs the remainder
    out[m, ] <- v
  }
  out
}

.amb_trend_vol_status_created  <- .amb_split_monthly(.amb_trend_vol_created,  .amb_status_probs,     101)
.amb_trend_val_status_created  <- .amb_split_monthly(.amb_trend_val_created,  .amb_status_val_probs, 202)
.amb_trend_vol_status_incurred <- .amb_split_monthly(.amb_trend_vol_incurred, .amb_status_probs,     303)
.amb_trend_val_status_incurred <- .amb_split_monthly(.amb_trend_val_incurred, .amb_status_val_probs, 404)

# Package as {status: {vol: [...12], val: [...12]}} for the trend chart JS.
.amb_trend_payload <- function(vol_total, val_total, vol_mat, val_mat) {
  out <- list(submitted = list(vol = vol_total, val = val_total))
  for (st in .AMB_PIPELINE_STATUSES) out[[st]] <- list(vol = vol_mat[, st], val = val_mat[, st])
  out
}

amb_trend_created  <- .amb_trend_payload(.amb_trend_vol_created,  .amb_trend_val_created,
                                          .amb_trend_vol_status_created, .amb_trend_val_status_created)
amb_trend_incurred <- .amb_trend_payload(.amb_trend_vol_incurred, .amb_trend_val_incurred,
                                          .amb_trend_vol_status_incurred, .amb_trend_val_status_incurred)

# ---- Chart.js head content --------------------------------------------------

amb_chart_js <- paste0(
  "var ambMonths=",          jsonlite::toJSON(.amb_months), ";",
  "var ambTrendCreated=",    jsonlite::toJSON(amb_trend_created), ";",
  "var ambTrendIncurred=",   jsonlite::toJSON(amb_trend_incurred), ";",
  "var ambAgeLabels=",       jsonlite::toJSON(.AMB_AGE_BUCKET_LABELS), ";",
  "var ambAgeColors=",       jsonlite::toJSON(.AMB_AGE_BUCKET_COLORS), ";",
  "var ambAgeCntCreated=",   jsonlite::toJSON(amb_aging$created$cnt), ";",
  "var ambAgeValCreated=",   jsonlite::toJSON(amb_aging$created$val), ";",
  "var ambAgeCntIncurred=",  jsonlite::toJSON(amb_aging$incurred$cnt), ";",
  "var ambAgeValIncurred=",  jsonlite::toJSON(amb_aging$incurred$val), ";",
  "var ambInterventionLabels=", jsonlite::toJSON(amb_by_intervention$intervention), ";",
  "var ambInterventionCnt=",    jsonlite::toJSON(amb_by_intervention$total_claims), ";",
  "var ambInterventionVal=",    jsonlite::toJSON(round(amb_by_intervention$total_value)), ";",
  "
  var _ambChartsReady = false;
  var _ambTrendChart = null, _ambAgeCountChart = null, _ambAgeValueChart = null, _ambInterventionChart = null;
  var _ambInterventionMetric = 'count';
  var _ambTrendDateMode = 'created', _ambTrendStatus = 'submitted';

  function _ambTrendSeries() {
    var src = _ambTrendDateMode === 'incurred' ? ambTrendIncurred : ambTrendCreated;
    return src[_ambTrendStatus] || src.submitted;
  }

  function initAmbCharts() {
    if (_ambChartsReady || typeof Chart === 'undefined') return;

    var ctxTrend = document.getElementById('ambTrendChart');
    if (ctxTrend) {
      var trendInit = _ambTrendSeries();
      _ambTrendChart = new Chart(ctxTrend, {
        data: {
          labels: ambMonths,
          datasets: [
            { type: 'bar', label: 'Claims Volume', data: trendInit.vol,
              backgroundColor: 'rgba(2,132,199,0.7)', borderColor: '#0284c7',
              borderWidth: 1, borderRadius: 4, yAxisID: 'y' },
            { type: 'line', label: 'Claims Value (KES)', data: trendInit.val,
              borderColor: '#f59e0b', backgroundColor: '#f59e0b',
              tension: 0.35, pointRadius: 4, pointHoverRadius: 6,
              borderWidth: 2, yAxisID: 'y1' }
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
                  if (c.dataset.yAxisID === 'y1')
                    return ' Value: KES ' + (c.parsed.y / 1e6).toFixed(1) + 'M';
                  return ' Volume: ' + c.parsed.y.toLocaleString() + ' claims';
                }
              }
            }
          },
          scales: {
            y:  { position: 'left', beginAtZero: true,
                  title: { display: true, text: 'Claims', color: '#64748b' },
                  grid: { color: '#f1f5f9' }, ticks: { color: '#64748b' } },
            y1: { position: 'right', beginAtZero: true,
                  title: { display: true, text: 'Value (KES)', color: '#64748b' },
                  grid: { drawOnChartArea: false },
                  ticks: { color: '#64748b',
                    callback: function(v) { return (v / 1e6).toFixed(0) + 'M'; } } },
            x:  { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    var ageBarBase = { borderRadius: 4, borderSkipped: false };

    var ctxAgeCnt = document.getElementById('ambAgeCountChart');
    if (ctxAgeCnt) {
      _ambAgeCountChart = new Chart(ctxAgeCnt, {
        type: 'bar',
        data: { labels: ambAgeLabels, datasets: [Object.assign({}, ageBarBase, {
          label: 'Claims', data: ambAgeCntCreated, backgroundColor: ambAgeColors
        })] },
        options: {
          responsive: true, maintainAspectRatio: false,
          plugins: {
            legend: { display: false },
            tooltip: { callbacks: { label: function(c) {
              return ' ' + c.parsed.y.toLocaleString() + ' claims';
            } } }
          },
          scales: {
            y: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b' } },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    var ctxAgeVal = document.getElementById('ambAgeValueChart');
    if (ctxAgeVal) {
      _ambAgeValueChart = new Chart(ctxAgeVal, {
        type: 'bar',
        data: { labels: ambAgeLabels, datasets: [Object.assign({}, ageBarBase, {
          label: 'Value (KES)', data: ambAgeValCreated, backgroundColor: ambAgeColors
        })] },
        options: {
          responsive: true, maintainAspectRatio: false,
          plugins: {
            legend: { display: false },
            tooltip: { callbacks: { label: function(c) {
              return ' KES ' + (c.parsed.y / 1e6).toFixed(1) + 'M';
            } } }
          },
          scales: {
            y: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b',
                   callback: function(v) { return 'KES ' + (v / 1e6).toFixed(0) + 'M'; } } },
            x: { grid: { display: false }, ticks: { color: '#64748b' } }
          }
        }
      });
    }

    var ctxIntervention = document.getElementById('ambInterventionChart');
    if (ctxIntervention) {
      _ambInterventionChart = new Chart(ctxIntervention, {
        type: 'bar',
        data: {
          labels: ambInterventionLabels,
          datasets: [{
            label: 'Claims',
            data: ambInterventionCnt,
            backgroundColor: 'rgba(2,132,199,0.7)',
            borderColor: '#0284c7',
            borderWidth: 1, borderRadius: 4, borderSkipped: false,
            maxBarThickness: 18
          }]
        },
        options: {
          indexAxis: 'y',
          responsive: true, maintainAspectRatio: false,
          plugins: {
            legend: { display: false },
            tooltip: { callbacks: { label: function(c) {
              return _ambInterventionMetric === 'amount'
                ? ' KES ' + (c.parsed.x / 1e6).toFixed(1) + 'M'
                : ' ' + c.parsed.x.toLocaleString() + ' claims';
            } } }
          },
          scales: {
            x: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b',
                   callback: function(v) {
                     return _ambInterventionMetric === 'amount' ? (v / 1e6).toFixed(0) + 'M' : v.toLocaleString();
                   } } },
            y: { grid: { display: false }, ticks: { color: '#64748b', autoSkip: false } }
          }
        }
      });
    }

    _ambChartsReady = true;
  }

  function switchAmbInterventionMetric(metric) {
    _ambInterventionMetric = metric;
    if (_ambInterventionChart) {
      var ds = _ambInterventionChart.data.datasets[0];
      ds.data = metric === 'amount' ? ambInterventionVal : ambInterventionCnt;
      ds.backgroundColor = metric === 'amount' ? 'rgba(245,158,11,0.75)' : 'rgba(2,132,199,0.7)';
      ds.borderColor     = metric === 'amount' ? '#f59e0b' : '#0284c7';
      _ambInterventionChart.update();
    }
    _ambToggleActive('intervention-metric', metric);
  }

  /* Local toggles for the trend chart - date mode and status each swap the
     underlying dataset in place; the two selections combine independently. */
  function _ambApplyTrend() {
    if (!_ambTrendChart) return;
    var d = _ambTrendSeries();
    _ambTrendChart.data.datasets[0].data = d.vol;
    _ambTrendChart.data.datasets[1].data = d.val;
    _ambTrendChart.update();
  }

  function _ambToggleActive(groupId, value) {
    document.querySelectorAll('.amb-date-toggle[data-group=\"' + groupId + '\"] button').forEach(function(b) {
      var active = b.getAttribute('data-mode') === value;
      b.classList.toggle('btn-primary', active);
      b.classList.toggle('btn-outline-secondary', !active);
    });
  }

  function switchAmbTrendDate(mode) {
    _ambTrendDateMode = mode;
    _ambApplyTrend();
    _ambToggleActive('trend-date', mode);
  }

  function switchAmbTrendStatus(status) {
    _ambTrendStatus = status;
    _ambApplyTrend();
    _ambToggleActive('trend-status', status);
  }

  function switchAmbAging(mode) {
    if (_ambAgeCountChart) {
      _ambAgeCountChart.data.datasets[0].data = mode === 'incurred' ? ambAgeCntIncurred : ambAgeCntCreated;
      _ambAgeCountChart.update();
    }
    if (_ambAgeValueChart) {
      _ambAgeValueChart.data.datasets[0].data = mode === 'incurred' ? ambAgeValIncurred : ambAgeValCreated;
      _ambAgeValueChart.update();
    }
    _ambToggleActive('aging', mode);
  }

  window.switchAmbTrendDate   = switchAmbTrendDate;
  window.switchAmbTrendStatus = switchAmbTrendStatus;
  window.switchAmbAging       = switchAmbAging;
  window.switchAmbInterventionMetric = switchAmbInterventionMetric;
  window.revealAmbCharts      = initAmbCharts;
  "
)

# ---- Component builders -----------------------------------------------------

.amb_metric_card <- function(label, value, icon_cls, color, sub_value = NULL, extra = NULL) {
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
          div(class = "fw-bold lh-sm", style = "font-size:1.5rem; color:#0f172a;", value),
          if (!is.null(sub_value))
            div(class = "text-muted mt-1", style = "font-size:.85rem;", sub_value),
          if (!is.null(extra)) div(class = "mt-2 d-flex gap-1 flex-wrap", extra)
        )
      )
    )
  )
}

.AMB_STATUS_ICONS <- c(
  submitted     = "bi bi-collection-fill",
  under_review  = "bi bi-search",
  returned      = "bi bi-reply-fill",
  rejected      = "bi bi-x-circle-fill",
  paid          = "bi bi-check-circle-fill"
)

# One card per status - `mode` is "count" (fmt_num) or "amount" (fmt_currency).
# Rejected gets the matching rejection-rate badge (% of claims in count mode,
# % of value in amount mode) since that's the one status where the two
# denominators are worth comparing side by side.
.amb_status_cards <- function(s, mode = c("count", "amount")) {
  mode <- match.arg(mode)
  fmt  <- if (mode == "count") fmt_num else fmt_currency
  do.call(tagList, lapply(.AMB_STATUSES, function(st) {
    val   <- s[[paste0(st, if (mode == "count") "_claims" else "_value")]]
    extra <- if (st == "rejected")
      pct_badge(if (mode == "count") s$rejection_rate_cnt else s$rejection_rate_val,
                8, 15, higher_is_bad = TRUE)
    .amb_metric_card(.AMB_STATUS_LABELS[[st]], fmt(val),
                     .AMB_STATUS_ICONS[[st]], .AMB_STATUS_COLORS[[st]],
                     extra = extra)
  }))
}

.amb_date_toggle <- function(group_id, js_fn, active = "created") {
  tags$div(class = "btn-group btn-group-sm amb-date-toggle", `data-group` = group_id, role = "group",
    tags$button(type = "button",
      class = paste("btn", if (active == "created") "btn-primary" else "btn-outline-secondary"),
      `data-mode` = "created",
      onclick = sprintf("%s('created')", js_fn),
      "Created Date"),
    tags$button(type = "button",
      class = paste("btn", if (active == "incurred") "btn-primary" else "btn-outline-secondary"),
      `data-mode` = "incurred",
      onclick = sprintf("%s('incurred')", js_fn),
      "Incurred Date")
  )
}

.amb_status_toggle <- function(active = "submitted") {
  tags$div(class = "btn-group btn-group-sm amb-date-toggle", `data-group` = "trend-status", role = "group",
    lapply(.AMB_STATUSES, function(st) {
      tags$button(type = "button",
        class = paste("btn", if (active == st) "btn-primary" else "btn-outline-secondary"),
        `data-mode` = st,
        onclick = sprintf("switchAmbTrendStatus('%s')", st),
        .AMB_STATUS_LABELS[[st]])
    })
  )
}

.amb_metric_toggle <- function(group_id, js_fn, active = "count") {
  tags$div(class = "btn-group btn-group-sm amb-date-toggle", `data-group` = group_id, role = "group",
    tags$button(type = "button",
      class = paste("btn", if (active == "count") "btn-primary" else "btn-outline-secondary"),
      `data-mode` = "count",
      onclick = sprintf("%s('count')", js_fn),
      "Count"),
    tags$button(type = "button",
      class = paste("btn", if (active == "amount") "btn-primary" else "btn-outline-secondary"),
      `data-mode` = "amount",
      onclick = sprintf("%s('amount')", js_fn),
      "Amount")
  )
}

.amb_chart_card <- function(title, subtitle, canvas_id, toggle = NULL, height = "280px") {
  div(class = "card border-0 shadow-sm h-100",
    div(class = "card-header bg-white border-bottom px-4 py-3 d-flex justify-content-between align-items-start flex-wrap gap-2",
      div(
        div(class = "fw-semibold", style = "color:#0f172a;", title),
        if (!is.null(subtitle)) div(class = "text-muted", style = "font-size:.82rem;", subtitle)
      ),
      toggle
    ),
    div(class = "card-body px-4 py-3",
      div(style = paste0("position:relative; height:", height, ";"), tags$canvas(id = canvas_id))
    )
  )
}

.amb_group_table <- function(df, group_col, group_label, code_col = NULL, code_label = NULL) {
  df <- df[order(-df$total_claims), ]
  if (nrow(df) == 0) {
    return(div(class = "text-center text-muted p-5",
      tags$i(class = "bi bi-funnel display-6 d-block mb-2 opacity-50"),
      "No records match the current filters."
    ))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    tags$tr(
      if (!is.null(code_col)) tags$td(tags$code(r[[code_col]])),
      tags$td(class = "fw-medium", r[[group_col]]),
      tags$td(fmt_num(r$total_claims)),
      tags$td(fmt_currency(r$total_value)),
      tags$td(title = paste0(fmt_num(r$cnt_rejected), " claims"), style = "cursor:default;",
              pct_badge(r$rejection_rate_cnt, 8, 15, higher_is_bad = TRUE)),
      tags$td(title = fmt_currency(r$val_rejected), style = "cursor:default;",
              pct_badge(r$rejection_rate_val, 8, 15, higher_is_bad = TRUE)),
      tags$td(paste0(r$avg_tat, " d"))
    )
  })
  div(class = "table-responsive",
    tags$table(
      class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          if (!is.null(code_col)) col_th(code_label, "110px"),
          col_th(group_label, "200px"),
          col_th("Volume"),
          col_th("Value (KES)"),
          col_th("Rejection Rate (% Claims)"),
          col_th("Rejection Rate (% Value)"),
          col_th("Avg TAT")
        )
      ),
      tags$tbody(do.call(tagList, rows))
    )
  )
}

# ---- Filter bar --------------------------------------------------------------

.amb_filter_bar <- function() {
  div(class = "card filter-card border-0 shadow-sm mb-4",
    div(class = "card-body py-3 px-4",
      div(class = "row g-3 align-items-end",

        div(class = "col-12",
          div(class = "filter-section-header",
            tags$i(class = "bi bi-sliders2"), "Filters"
          )
        ),

        div(class = "col-12 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label", tags$i(class = "bi bi-calendar3"), "Created Date"),
            selectInput("amb_created_month", NULL,
              choices  = c("All Months" = "", setNames(.amb_months, .amb_months)),
              selected = "", width = "100%")
          )
        ),

        div(class = "col-12 col-md-2 col-xl-2",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label", tags$i(class = "bi bi-calendar-check"), "Incurred Date"),
            selectInput("amb_incurred_month", NULL,
              choices  = c("All Months" = "", setNames(.amb_months, .amb_months)),
              selected = "", width = "100%")
          )
        ),

        div(class = "col-12 col-md-3 col-xl-3",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label", tags$i(class = "bi bi-geo-alt"), "County"),
            selectInput("amb_county", NULL,
              choices  = c("All Counties" = "", setNames(.AMB_COUNTIES, .AMB_COUNTIES)),
              multiple = TRUE, selectize = TRUE, width = "100%")
          )
        ),

        div(class = "col-12 col-md-3 col-xl-3",
          div(class = "filter-bar-group",
            div(class = "filter-bar-label", tags$i(class = "bi bi-truck-front"), "Vendor"),
            selectInput("amb_vendor", NULL,
              choices  = c("All Vendors" = "", setNames(.AMB_VENDORS, .AMB_VENDORS)),
              multiple = TRUE, selectize = TRUE, width = "100%")
          )
        ),

        div(class = "col-12 col-xl-2",
          div(class = "filter-bar-label", style = "visibility:hidden;", HTML("&nbsp;")),
          div(class = "d-flex gap-2 justify-content-end",
            actionButton("amb_reset",
              label = tagList(tags$i(class = "bi bi-arrow-counterclockwise"), " Reset"),
              class = "btn btn-filter-reset", title = "Reset filters"),
            actionButton("amb_apply",
              label = tagList(tags$i(class = "bi bi-funnel-fill me-1"), "Apply Filters"),
              class = "btn btn-primary btn-filter-apply")
          )
        )
      )
    )
  )
}

# ---- Public: panel UI --------------------------------------------------------

amb_panel_ui <- function() {
  s <- amb_summary

  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "Ambulance Claims",
      last_updated = "3 Aug 2026",
      source       = "Payer System + ERP",
      info         = paste0(
        "Claims for ambulance/EMS transport, tracked across four pipeline statuses - ",
        "Under Review, Returned, Rejected and Paid. Submitted is the total across all ",
        "four. Total Submitted = Paid + Rejected + Outstanding, where Outstanding = ",
        "Under Review + Returned."
      ),
      badges = tagList(
        tags$span(class = "badge text-bg-primary px-3 py-2 rounded-pill",   "DHA Leadership"),
        tags$span(class = "badge text-bg-secondary px-3 py-2 rounded-pill", "Governors"),
        tags$span(class = "badge text-bg-dark px-3 py-2 rounded-pill",      "Facility Executive")
      )
    ),

    insight_banner(
      paste0(
        fmt_num(s$unique_patients), " unique patients across ", fmt_num(s$unique_vendors),
        " unique vendors."
      ),
      type = "info"
    ),

    .amb_filter_bar(),

    # Count - one card per status
    tags$h6(class = "ind-section-label", "Count"),
    div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-5 g-3",
      .amb_status_cards(s, "count")
    ),

    tags$hr(class = "my-4 border-light"),

    # Amount - one card per status
    tags$h6(class = "ind-section-label", "Amount"),
    div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-5 g-3",
      .amb_status_cards(s, "amount")
    ),

    tags$hr(class = "my-4 border-light"),

    # Trend chart
    tags$h6(class = "ind-section-label", "Monthly Trend (last 12 months)"),
    .amb_chart_card(
      "Claims Volume & Value", "Toggle status and Created/Incurred date",
      "ambTrendChart",
      toggle = div(class = "d-flex flex-column align-items-end gap-2",
        .amb_status_toggle(),
        .amb_date_toggle("trend-date", "switchAmbTrendDate")
      ),
      height = "300px"
    ),

    tags$hr(class = "my-4 border-light"),

    # Turnaround time
    tags$h6(class = "ind-section-label",
      "Turnaround Time — Submission to Payment"),
    div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-4 g-3",
      .amb_metric_card("Average TAT", paste0(s$avg_tat, " days"),
                       "bi bi-stopwatch-fill", "#0ea5e9"),
      .amb_metric_card("Median TAT", paste0(s$median_tat, " days"),
                       "bi bi-bar-chart-line-fill", "#f59e0b"),
      .amb_metric_card("Minimum TAT", paste0(s$min_tat, " days"),
                       "bi bi-lightning-charge-fill", "#22d3ee"),
      .amb_metric_card("Maximum TAT", paste0(s$max_tat, " days"),
                       "bi bi-exclamation-circle-fill", "#ef4444")
    ),

    tags$hr(class = "my-4 border-light"),

    # Aging bucket charts
    tags$h6(class = "ind-section-label", "Aging of Pending Claims"),
    div(class = "d-flex justify-content-end mb-2",
        .amb_date_toggle("aging", "switchAmbAging")),
    div(class = "row g-4",
      div(class = "col-12 col-xl-6",
        .amb_chart_card("Number of Pending Claims by Age Bucket", NULL, "ambAgeCountChart")),
      div(class = "col-12 col-xl-6",
        .amb_chart_card("Value of Pending Claims by Age Bucket", NULL, "ambAgeValueChart"))
    ),

    tags$hr(class = "my-4 border-light"),

    # County / Vendor / Intervention table
    tags$h6(class = "ind-section-label", "Breakdown by County / Vendor / Intervention"),
    tags$ul(class = "nav nav-pills mb-3", role = "tablist",
      tags$li(class = "nav-item", role = "presentation",
        tags$button(class = "nav-link active", id = "amb-tab-county",
          `data-bs-toggle` = "pill", `data-bs-target` = "#amb-pane-county",
          type = "button", role = "tab", "By County")),
      tags$li(class = "nav-item", role = "presentation",
        tags$button(class = "nav-link", id = "amb-tab-vendor",
          `data-bs-toggle` = "pill", `data-bs-target` = "#amb-pane-vendor",
          type = "button", role = "tab", "By Vendor")),
      tags$li(class = "nav-item", role = "presentation",
        tags$button(class = "nav-link", id = "amb-tab-intervention",
          `data-bs-toggle` = "pill", `data-bs-target` = "#amb-pane-intervention",
          type = "button", role = "tab", "By Intervention"))
    ),
    div(class = "card border-0 shadow-sm",
      div(class = "card-header bg-white border-bottom d-flex justify-content-between align-items-center py-3 px-4",
        div(class = "fw-semibold", style = "color:#0f172a;", "Vendor / County / Intervention Performance"),
        tags$button(type = "button",
          class = "btn btn-sm btn-outline-secondary",
          onclick = "showMockAction('Preparing ambulance claims Excel export - the file will download shortly.')",
          tags$i(class = "bi bi-file-earmark-excel me-1"), "Export"
        )
      ),
      div(class = "tab-content",
        div(id = "amb-pane-county", class = "tab-pane fade show active", role = "tabpanel",
          .amb_group_table(amb_by_county, "county", "County")),
        div(id = "amb-pane-vendor", class = "tab-pane fade", role = "tabpanel",
          .amb_group_table(amb_by_vendor, "vendor", "Vendor")),
        div(id = "amb-pane-intervention", class = "tab-pane fade", role = "tabpanel",
          .amb_group_table(amb_by_intervention, "intervention", "Intervention",
                           code_col = "code", code_label = "SHA Code"))
      ),
      div(class = "card-footer bg-white border-top",
        tags$span(class = "text-muted small",
          sprintf("%d vendors · %d counties · %d interventions",
                  nrow(amb_by_vendor), nrow(amb_by_county), nrow(amb_by_intervention)))
      )
    ),

    tags$hr(class = "my-4 border-light"),

    # Claims by intervention (SHA benefit-package line)
    tags$h6(class = "ind-section-label", "Claims by Intervention"),
    .amb_chart_card(
      "Volume / Value by Intervention", "SHA benefit-package line item that triggered the dispatch",
      "ambInterventionChart",
      toggle = .amb_metric_toggle("intervention-metric", "switchAmbInterventionMetric"),
      height = "380px"
    )
  )
}

# ---- Public: server ----------------------------------------------------------

amb_server <- function(input, output, session) {
  observeEvent(input$amb_reset, {
    updateSelectInput(session, "amb_created_month",  selected = "")
    updateSelectInput(session, "amb_incurred_month", selected = "")
    updateSelectInput(session, "amb_county",         selected = character(0))
    updateSelectInput(session, "amb_vendor",         selected = character(0))
  })
}

# ---- Data Model samples -------------------------------------------------------

amb_raw_sample <- data.frame(
  claim_id      = c("AMB-19301","AMB-19302","AMB-19303","AMB-19304","AMB-19305"),
  vendor        = c("St John Ambulance Kenya","Kenya Red Cross Ambulance",
                    "AMREF Flying Doctors","Rescue.co Kenya","Nairobi County EMS"),
  county        = c("Nairobi","Mombasa","Kisumu","Nakuru","Eldoret"),
  patient_id    = c("PAT-88213","PAT-88214","PAT-88215","PAT-88216","PAT-88217"),
  date_incurred = c("2026-06-02","2026-06-04","2026-06-03","2026-06-06","2026-06-05"),
  date_created  = c("2026-06-03","2026-06-06","2026-06-04","2026-06-09","2026-06-06"),
  claim_amount  = c(24500,31200,18900,42300,27600),
  status        = c("paid","rejected","under_review","returned","paid"),
  date_paid     = c("2026-06-11","NA","NA","NA","2026-06-15"),
  stringsAsFactors = FALSE
)

amb_modelled_sample <- data.frame(
  vendor              = .AMB_VENDORS,
  total_claims        = amb_by_vendor$total_claims,
  total_value_KES     = round(amb_by_vendor$total_value),
  paid_claims         = amb_by_vendor$cnt_paid,
  rejected_claims     = amb_by_vendor$cnt_rejected,
  outstanding_claims  = amb_by_vendor$outstanding_claims,
  rejection_rate_cnt  = amb_by_vendor$rejection_rate_cnt,
  rejection_rate_val  = amb_by_vendor$rejection_rate_val,
  avg_tat_days        = amb_by_vendor$avg_tat,
  stringsAsFactors = FALSE
)
