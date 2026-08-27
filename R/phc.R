# ==============================================================================
# Indicator: PHC Claims & Allocation
# Spec: requirements/phc_claims_allocation.md
# Source template: requirements/other/phc-table-templates[21].docx
#   (Digital Health Agency standard table templates for Primary Health Care)
# ==============================================================================

# ---- Constants ----------------------------------------------------------------

# All 47 counties, in the standard county-code order used by the source docx.
.PHC_COUNTIES <- c(
  "Mombasa","Kwale","Kilifi","Tana River","Lamu","Taita Taveta","Garissa","Wajir",
  "Mandera","Marsabit","Isiolo","Meru","Tharaka Nithi","Embu","Kitui","Machakos",
  "Makueni","Nyandarua","Nyeri","Kirinyaga","Murang'a","Kiambu","Turkana","West Pokot",
  "Samburu","Trans Nzoia","Uasin Gishu","Elgeyo Marakwet","Nandi","Baringo","Laikipia",
  "Nakuru","Narok","Kajiado","Kericho","Bomet","Kakamega","Vihiga","Bungoma","Busia",
  "Siaya","Kisumu","Homa Bay","Migori","Kisii","Nyamira","Nairobi City"
)

.PHC_LEVELS <- c("l2", "l3", "l4", "l5", "l6", "lnc")
.PHC_LEVEL_LABELS <- c(
  l2  = "Level 2 — Dispensary",
  l3  = "Level 3 — Health centre",
  l4  = "Level 4 — Primary / county hospital",
  l5  = "Level 5 — County referral hospital",
  l6  = "Level 6 — National referral hospital",
  lnc = "Level not classified"
)

.PHC_OWNERSHIP <- c("public", "fbo", "private", "ngo", "ns")
.PHC_OWNERSHIP_LABELS <- c(
  public  = "Public (MoH / county government)",
  fbo     = "Faith-based (FBO)",
  private = "Private",
  ngo     = "NGO",
  ns      = "Ownership not stated"
)
.PHC_OWN_COST_MULT <- c(public = 0.90, fbo = 1.05, private = 1.35, ngo = 1.10, ns = 1.00)

.PHC_LEVEL_FAC_RANGE   <- list(l2 = c(6, 42),  l3 = c(3, 22),  l4 = c(1, 7),
                                l5 = c(0, 2),   l6 = c(0, 1),   lnc = c(0, 3))
.PHC_LEVEL_CPF_RANGE   <- list(l2 = c(80, 260), l3 = c(120, 380), l4 = c(200, 550),
                                l5 = c(350, 900), l6 = c(500, 1200), lnc = c(40, 150))
.PHC_LEVEL_COST_RANGE  <- list(l2 = c(700, 1200), l3 = c(900, 1500), l4 = c(1300, 2100),
                                l5 = c(1900, 3100), l6 = c(2600, 4200), lnc = c(1000, 1600))
.PHC_OWNERSHIP_MIX <- list(
  l2  = c(public = 0.55, fbo = 0.20, private = 0.15, ngo = 0.05, ns = 0.05),
  l3  = c(public = 0.50, fbo = 0.22, private = 0.18, ngo = 0.05, ns = 0.05),
  l4  = c(public = 0.65, fbo = 0.15, private = 0.12, ngo = 0.03, ns = 0.05),
  l5  = c(public = 0.85, fbo = 0.05, private = 0.05, ngo = 0.02, ns = 0.03),
  l6  = c(public = 0.90, fbo = 0.03, private = 0.04, ngo = 0.01, ns = 0.02),
  lnc = c(public = 0.30, fbo = 0.15, private = 0.15, ngo = 0.05, ns = 0.35)
)

.phc_months <- format(seq(as.Date("2025-07-01"), by = "month", length.out = 12), "%b '%y")

# ---- Data: county x level grid (base of Tables 1, 2 and 3) --------------------
# One seeded grid, three different rollups - this is what makes the headline
# cards and Tables 1-3 reconcile to each other by construction, per the
# source docx's own "fix the denominator before filling any cell" rule.

.make_phc_county_level <- function(county, level, ci, li) {
  set.seed(ci * 97 + li * 31)
  fr <- .PHC_LEVEL_FAC_RANGE[[level]]
  facilities <- sample(fr[1]:fr[2], 1)
  if (facilities == 0) {
    return(data.frame(county = county, level = level, facilities = 0L,
                       claims = 0L, amount = 0, unique_members = 0L,
                       stringsAsFactors = FALSE))
  }
  cpf <- .PHC_LEVEL_CPF_RANGE[[level]]
  claims <- as.integer(sum(round(runif(facilities, cpf[1], cpf[2]))))
  cost_range <- .PHC_LEVEL_COST_RANGE[[level]]
  avg_cost   <- runif(1, cost_range[1], cost_range[2])
  amount     <- claims * avg_cost * runif(1, 0.9, 1.1)
  unique_members <- as.integer(round(claims / runif(1, 1.15, 1.65)))
  data.frame(county = county, level = level, facilities = as.integer(facilities),
             claims = claims, amount = amount, unique_members = unique_members,
             stringsAsFactors = FALSE)
}

phc_base_grid <- do.call(rbind, unlist(lapply(seq_along(.PHC_COUNTIES), function(ci) {
  lapply(seq_along(.PHC_LEVELS), function(li) {
    .make_phc_county_level(.PHC_COUNTIES[ci], .PHC_LEVELS[li], ci, li)
  })
}), recursive = FALSE))

# ---- Table 1: facility level x ownership, national -----------------------------

phc_level_totals <- aggregate(cbind(facilities, claims, amount, unique_members) ~ level,
                               data = phc_base_grid, FUN = sum)
phc_level_totals <- phc_level_totals[match(.PHC_LEVELS, phc_level_totals$level), ]

.phc_split_ownership <- function(level, li) {
  tot <- phc_level_totals[phc_level_totals$level == level, ]
  set.seed(li * 211)
  mix <- .PHC_OWNERSHIP_MIX[[level]][.PHC_OWNERSHIP]
  mix <- pmax(0.01, mix + runif(5, -0.02, 0.02)); mix <- mix / sum(mix)

  fac <- as.integer(floor(tot$facilities * mix)); fac[5] <- tot$facilities - sum(fac[1:4])
  cl  <- as.integer(floor(tot$claims     * mix)); cl[5]  <- tot$claims     - sum(cl[1:4])

  raw_amt <- cl * .PHC_OWN_COST_MULT[.PHC_OWNERSHIP]
  amt <- if (sum(raw_amt) > 0) raw_amt / sum(raw_amt) * tot$amount else rep(0, 5)

  data.frame(level = level, ownership = .PHC_OWNERSHIP,
             facilities = fac, claims = cl, amount = amt,
             stringsAsFactors = FALSE)
}

phc_table1 <- do.call(rbind, lapply(seq_along(.PHC_LEVELS), function(li) {
  .phc_split_ownership(.PHC_LEVELS[li], li)
}))
phc_table1$amount_per_claim <- ifelse(phc_table1$claims > 0, phc_table1$amount / phc_table1$claims, 0)

phc_national_facilities <- sum(phc_level_totals$facilities)
phc_national_claims     <- sum(phc_level_totals$claims)
phc_national_amount     <- sum(phc_level_totals$amount)

# ---- Table 2: facilities, claims, amount by county ------------------------------

phc_table2 <- aggregate(cbind(facilities, claims, amount, unique_members) ~ county,
                         data = phc_base_grid, FUN = sum)
phc_table2 <- phc_table2[match(.PHC_COUNTIES, phc_table2$county), ]
phc_table2$amount_per_claim <- ifelse(phc_table2$claims > 0, phc_table2$amount / phc_table2$claims, 0)
phc_table2$share_pct <- round(phc_table2$amount / phc_national_amount * 100, 1)

# A member claiming in more than one county is counted once per county and
# once nationally, so county unique-member counts do not sum to the national
# total (per the docx's own note on Table 2). Modelled as a fixed overlap
# discount applied when rolling county figures up to the national headline.
.PHC_MEMBER_OVERLAP_FACTOR <- 0.94
phc_national_unique_members <- round(sum(phc_table2$unique_members) * .PHC_MEMBER_OVERLAP_FACTOR)

# ---- Table 3: total amount by county x facility level ---------------------------

phc_table3 <- data.frame(county = .PHC_COUNTIES, stringsAsFactors = FALSE)
for (lv in .PHC_LEVELS) {
  sub <- phc_base_grid[phc_base_grid$level == lv, c("county", "amount")]
  phc_table3[[lv]] <- sub$amount[match(phc_table3$county, sub$county)]
}
phc_table3$total <- rowSums(phc_table3[.PHC_LEVELS])

# ---- Headline indicators ---------------------------------------------------------

phc_summary <- list(
  total_amount      = phc_national_amount,
  total_facilities  = phc_national_facilities,
  total_claims      = phc_national_claims,
  unique_members    = phc_national_unique_members,
  period_months     = 12
)
phc_summary$visit_frequency <- round(
  phc_summary$total_claims / phc_summary$unique_members / phc_summary$period_months, 2
)

# ---- Table 4: monthly allocation by facility x payment status -------------------
# Sits on a different base to Tables 1-3 (month of allocation, not month of
# service), so it is not required to reconcile to the headline total - per
# the docx's own caption rule. Five representative facilities are shown, with
# a National total block; the docx itself uses the same "show a handful, note
# it repeats for every facility" convention for this table.

.PHC_SAMPLE_FACILITIES <- data.frame(
  name   = c("Kibera Level 2 Dispensary", "Mathare North Health Centre",
             "Nakuru Level 4 Hospital", "Kisumu Central Health Centre",
             "Kilifi County Referral Hospital"),
  county = c("Nairobi City", "Nairobi City", "Nakuru", "Kisumu", "Kilifi"),
  level  = c("l2", "l3", "l4", "l3", "l5"),
  stringsAsFactors = FALSE
)

.make_phc_facility_months <- function(idx, level) {
  set.seed(idx * 173)
  base_range <- switch(level, l2 = c(300, 900), l3 = c(600, 1800),
                        l4 = c(1200, 3600), l5 = c(2500, 6500), c(300, 900))
  all_amt  <- round(runif(12, base_range[1], base_range[2]))
  notpaid_share <- pmax(0.05, pmin(0.35, runif(12, 0.08, 0.22)))
  notpaid <- round(all_amt * notpaid_share)
  paid    <- all_amt - notpaid
  list(paid = paid, notpaid = notpaid, all = all_amt)
}

phc_table4_facilities <- lapply(seq_len(nrow(.PHC_SAMPLE_FACILITIES)), function(i) {
  f <- .PHC_SAMPLE_FACILITIES[i, ]
  m <- .make_phc_facility_months(i, f$level)
  list(name = f$name, county = f$county, level = .PHC_LEVEL_LABELS[[f$level]],
       cid = paste0("c-phc-fac-", i), months = m,
       total_paid = sum(m$paid), total_notpaid = sum(m$notpaid), total_all = sum(m$all))
})

set.seed(919)
.phc_national_all_monthly <- round(runif(12, 180000, 420000))
.phc_national_notpaid_share <- pmax(0.06, pmin(0.18, runif(12, 0.08, 0.14)))
phc_table4_national <- list(
  all     = .phc_national_all_monthly,
  notpaid = round(.phc_national_all_monthly * .phc_national_notpaid_share)
)
phc_table4_national$paid <- phc_table4_national$all - phc_table4_national$notpaid

# ---- Tables 5-8: diagnosis and intervention rankings -----------------------------
# Volume-only (no amount column), per the docx's explicit rule that mixing a
# count ranking with an amount column invites reading the list as a cost
# ranking, which it is not. Codes/titles are illustrative primary-care
# conditions, not a real ICD-11 stem-code extract - flagged as a placeholder
# in the requirements doc pending the real diagnosis dictionary mapping.

.PHC_DIAGNOSES <- data.frame(
  code = c("CA07.0","1F03","GC30.0","BA00.Z","JA63.0","5A11","DA63.2","1A00.Z",
           "DB31.0","CA40.1","1G40","QA21.1","FA05.1","9A76.0","DD91.0","4A44.Z",
           "MG24","EK02","QA02.0","MD90.0"),
  title = c(
    "Acute upper respiratory tract infection", "Malaria, uncomplicated",
    "Essential (primary) hypertension", "Urinary tract infection, unspecified",
    "Antenatal care — supervision of normal pregnancy", "Type 2 diabetes mellitus",
    "Acute gastroenteritis, presumed infectious", "Intestinal helminthiasis",
    "Skin and subcutaneous tissue infection", "Pneumonia, unspecified organism",
    "Dental caries", "Postnatal care follow-up", "Osteoarthritis, unspecified site",
    "Family planning — general counselling and care", "Peptic ulcer disease",
    "Conjunctivitis, unspecified", "Asthma", "Anaemia, unspecified",
    "Well-child / routine child health check", "Contact dermatitis"
  ),
  chapter = c(
    "Respiratory system", "Certain infectious/parasitic diseases", "Circulatory system",
    "Genitourinary system", "Pregnancy, childbirth and the puerperium", "Endocrine/nutritional/metabolic",
    "Digestive system", "Certain infectious/parasitic diseases", "Skin and subcutaneous tissue",
    "Respiratory system", "Diseases of the oral cavity", "Pregnancy, childbirth and the puerperium",
    "Musculoskeletal system", "Factors influencing health status", "Digestive system",
    "Diseases of the eye", "Respiratory system", "Diseases of the blood",
    "Factors influencing health status", "Skin and subcutaneous tissue"
  ),
  weight = c(19, 16, 10, 9, 8, 6, 6, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 1),
  stringsAsFactors = FALSE
)

.PHC_INTERVENTIONS <- data.frame(
  code = c("PHC-01","PHC-02","PHC-03","PHC-04","PHC-05","PHC-06","PHC-07","PHC-08",
           "PHC-09","PHC-10","PHC-11","PHC-12","PHC-13","PHC-14","PHC-15","PHC-16",
           "PHC-17","PHC-18","PHC-19","PHC-20"),
  name = c(
    "Outpatient consultation", "Nursing / clinical assessment",
    "Malaria testing (mRDT)", "Antenatal care visit",
    "Immunization — routine EPI", "Family planning — short-acting methods",
    "Blood pressure / diabetes screening", "Health education session",
    "Growth monitoring (under-5)", "Wound dressing / minor procedure",
    "Laboratory — basic diagnostics", "Nutrition assessment and counselling",
    "HIV testing services (HTS)", "Deworming", "Postnatal care visit",
    "TB screening", "Cervical cancer screening (VIA)", "Dental extraction",
    "Minor surgical procedure", "Referral facilitation"
  ),
  weight = c(20, 15, 11, 9, 8, 6, 6, 5, 5, 4, 4, 3, 3, 3, 2, 2, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

.make_phc_ranking <- function(dict, total_records, seed_base) {
  n <- nrow(dict)
  set.seed(seed_base)
  w <- pmax(0.2, dict$weight + runif(n, -1.5, 1.5))
  claims <- round(w / sum(w) * total_records * 0.78)   # top-N covers ~78%, rest is long tail
  claims[claims < 1] <- 1
  set.seed(seed_base + 1)
  members <- pmax(1, round(claims / runif(n, 1.05, 1.45)))

  out <- dict
  out$label <- if (!is.null(out$title)) out$title else out$name
  out$claims <- claims
  out$unique_members <- members
  out <- out[order(-out$claims), ]
  out$rank <- seq_len(n)
  out$pct <- round(out$claims / total_records * 100, 1)
  out$claims_per_member <- round(out$claims / out$unique_members, 2)
  list(rows = out, top_subtotal = sum(out$claims), all_total = total_records)
}

# Table 5 - national diagnosis ranking. ~72% of claims carry a coded primary
# diagnosis; the rest are excluded from the ranking (missing/invalid code).
phc_diag_national <- .make_phc_ranking(.PHC_DIAGNOSES, round(phc_summary$total_claims * 0.72), 901)

# Table 6 - three representative county panels (repeats for all 47 counties).
.PHC_DIAG_COUNTY_PANELS <- c("Nairobi City", "Kisumu", "Turkana")
phc_diag_by_county <- setNames(
  lapply(seq_along(.PHC_DIAG_COUNTY_PANELS), function(i) {
    co <- .PHC_DIAG_COUNTY_PANELS[i]
    co_claims <- phc_table2$claims[phc_table2$county == co]
    .make_phc_ranking(.PHC_DIAGNOSES, round(co_claims * 0.72), 1001 + i * 17)
  }),
  .PHC_DIAG_COUNTY_PANELS
)

# Table 7 - three representative facility panels (repeats for every
# transacting facility). Reuses the first 3 sample facilities from Table 4.
.PHC_DIAG_FACILITY_PANELS <- .PHC_SAMPLE_FACILITIES[1:3, ]
phc_diag_by_facility <- lapply(seq_len(nrow(.PHC_DIAG_FACILITY_PANELS)), function(i) {
  f <- .PHC_DIAG_FACILITY_PANELS[i, ]
  set.seed(2001 + i * 23)
  fac_claims <- sample(2200:8600, 1)
  list(name = f$name, county = f$county, level = .PHC_LEVEL_LABELS[[f$level]],
       ranking = .make_phc_ranking(.PHC_DIAGNOSES, round(fac_claims * 0.72), 2001 + i * 23))
})

# Table 8 - national intervention ranking. A claim can carry more than one
# intervention, so the record total exceeds the claim total.
phc_intervention_national <- .make_phc_ranking(.PHC_INTERVENTIONS, round(phc_summary$total_claims * 1.35), 3001)

# ---- Chart.js head content --------------------------------------------------------

.phc_top10_counties <- phc_table2[order(-phc_table2$amount), ][1:10, ]

phc_chart_js <- paste0(
  "var phcLevelLabels=", jsonlite::toJSON(unname(.PHC_LEVEL_LABELS)), ";",
  "var phcLevelAmount=", jsonlite::toJSON(round(phc_level_totals$amount)), ";",
  "var phcTopCountyLabels=", jsonlite::toJSON(.phc_top10_counties$county), ";",
  "var phcTopCountyAmount=", jsonlite::toJSON(round(.phc_top10_counties$amount)), ";",
  "
  var _phcChartsReady = false;
  function initPhcCharts() {
    if (_phcChartsReady || typeof Chart === 'undefined') return;

    var ctxLevel = document.getElementById('phcLevelChart');
    if (ctxLevel) {
      new Chart(ctxLevel, {
        type: 'bar',
        data: { labels: phcLevelLabels, datasets: [{
          label: 'Total amount (KES)', data: phcLevelAmount,
          backgroundColor: 'rgba(2,132,199,0.7)', borderColor: '#0284c7',
          borderWidth: 1, borderRadius: 4, borderSkipped: false
        }] },
        options: {
          responsive: true, maintainAspectRatio: false,
          plugins: { legend: { display: false },
            tooltip: { callbacks: { label: function(c) { return ' KES ' + (c.parsed.y/1e6).toFixed(1) + 'M'; } } } },
          scales: {
            y: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b',
                   callback: function(v) { return (v/1e6).toFixed(0) + 'M'; } } },
            x: { grid: { display: false }, ticks: { color: '#64748b', autoSkip: false,
                   maxRotation: 28, minRotation: 0, font: { size: 10 } } }
          }
        }
      });
    }

    var ctxCounty = document.getElementById('phcCountyChart');
    if (ctxCounty) {
      new Chart(ctxCounty, {
        type: 'bar',
        data: { labels: phcTopCountyLabels, datasets: [{
          label: 'Total amount (KES)', data: phcTopCountyAmount,
          backgroundColor: 'rgba(245,158,11,0.75)', borderColor: '#f59e0b',
          borderWidth: 1, borderRadius: 4, borderSkipped: false, maxBarThickness: 22
        }] },
        options: {
          indexAxis: 'y',
          responsive: true, maintainAspectRatio: false,
          plugins: { legend: { display: false },
            tooltip: { callbacks: { label: function(c) { return ' KES ' + (c.parsed.x/1e6).toFixed(1) + 'M'; } } } },
          scales: {
            x: { beginAtZero: true, grid: { color: '#f1f5f9' }, ticks: { color: '#64748b',
                   callback: function(v) { return (v/1e6).toFixed(0) + 'M'; } } },
            y: { grid: { display: false }, ticks: { color: '#64748b', autoSkip: false } }
          }
        }
      });
    }

    _phcChartsReady = true;
  }
  window.revealPhcCharts = initPhcCharts;
  "
)

# ---- Component builders: cards, tables ---------------------------------------------

.phc_metric_card <- function(label, value, icon_cls, color, sub_value = NULL) {
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

.phc_chart_card <- function(title, subtitle, canvas_id, height = "300px") {
  div(class = "card border-0 shadow-sm h-100",
    div(class = "card-header bg-white border-bottom px-4 py-3",
      div(class = "fw-semibold", style = "color:#0f172a;", title),
      if (!is.null(subtitle)) div(class = "text-muted", style = "font-size:.82rem;", subtitle)
    ),
    div(class = "card-body px-4 py-3",
      div(style = paste0("position:relative; height:", height, ";"), tags$canvas(id = canvas_id))
    )
  )
}

.phc_pct <- function(x) paste0(sprintf("%.1f", x), "%")

# Amount columns are precise KES '000 (matching the source docx's declared
# unit), never abbreviated to M/B via fmt_currency() - abbreviating would
# lose the precision the docx's table headers explicitly call for.
.phc_fmt_000 <- function(x) fmt_num(round(x / 1000))

# ---- Table 1 UI: level x ownership, all rows shown (no expand needed) --------------

.phc_table1_ui <- function() {
  rows <- lapply(seq_along(.PHC_LEVELS), function(li) {
    lv  <- .PHC_LEVELS[li]
    tot <- phc_level_totals[phc_level_totals$level == lv, ]

    header <- tags$tr(class = "table-secondary fw-semibold",
      tags$td(.PHC_LEVEL_LABELS[[lv]]),
      tags$td(fmt_num(tot$facilities)),
      tags$td(.phc_pct(tot$facilities / phc_national_facilities * 100)),
      tags$td(fmt_num(tot$claims)),
      tags$td(.phc_pct(tot$claims / phc_national_claims * 100)),
      tags$td(.phc_fmt_000(tot$amount)),
      tags$td(.phc_pct(tot$amount / phc_national_amount * 100)),
      tags$td(fmt_num(ifelse(tot$claims > 0, tot$amount / tot$claims, 0)))
    )

    children <- lapply(.PHC_OWNERSHIP, function(own) {
      r <- phc_table1[phc_table1$level == lv & phc_table1$ownership == own, ]
      tags$tr(
        tags$td(style = "padding-left:2rem;",
          tags$span(class = "text-secondary", style = "font-size:.9rem;", .PHC_OWNERSHIP_LABELS[[own]])),
        tags$td(class = "text-secondary", fmt_num(r$facilities)),
        tags$td(class = "text-muted", .phc_pct(r$facilities / phc_national_facilities * 100)),
        tags$td(class = "text-secondary", fmt_num(r$claims)),
        tags$td(class = "text-muted", .phc_pct(r$claims / phc_national_claims * 100)),
        tags$td(class = "text-secondary", .phc_fmt_000(r$amount)),
        tags$td(class = "text-muted", .phc_pct(r$amount / phc_national_amount * 100)),
        tags$td(class = "text-secondary", fmt_num(r$amount_per_claim))
      )
    })

    tagList(header, do.call(tagList, children))
  })

  all_row <- tags$tr(class = "table-secondary fw-semibold",
    tags$td("All levels"),
    tags$td(fmt_num(phc_national_facilities)), tags$td("100.0%"),
    tags$td(fmt_num(phc_national_claims)), tags$td("100.0%"),
    tags$td(.phc_fmt_000(phc_national_amount)), tags$td("100.0%"),
    tags$td(fmt_num(phc_national_amount / phc_national_claims))
  )

  div(class = "table-responsive",
    tags$table(class = "table table-hover align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          tags$th("Facility level and ownership"),
          tags$th("Facilities"), tags$th("%"),
          tags$th("Claims"), tags$th("%"),
          tags$th("Total amount (KES '000)"), tags$th("%"),
          tags$th("Amt / claim (KES)")
        )
      ),
      tags$tbody(do.call(tagList, rows), all_row)
    )
  )
}

# ---- Table 2 UI: by county (plain, sortable) -----------------------------------

.phc_table2_ui <- function() {
  rows <- lapply(seq_len(nrow(phc_table2)), function(i) {
    r <- phc_table2[i, ]
    tags$tr(
      tags$td(class = "fw-medium", r$county),
      tags$td(fmt_num(r$facilities)),
      tags$td(fmt_num(r$claims)),
      tags$td(.phc_fmt_000(r$amount)),
      tags$td(paste0(sprintf("%.1f", r$share_pct), "%")),
      tags$td(fmt_num(r$amount_per_claim)),
      tags$td(fmt_num(r$unique_members))
    )
  })
  national_row <- tags$tr(class = "table-secondary fw-semibold",
    tags$td("National"),
    tags$td(fmt_num(phc_national_facilities)),
    tags$td(fmt_num(phc_national_claims)),
    tags$td(.phc_fmt_000(phc_national_amount)),
    tags$td("100.0%"),
    tags$td(fmt_num(phc_national_amount / phc_national_claims)),
    tags$td(fmt_num(phc_national_unique_members))
  )
  div(class = "table-responsive", style = "max-height:520px; overflow-y:auto;",
    tags$table(class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("County", "160px"), col_th("Facilities"), col_th("Claims"),
          col_th("Total amount (KES '000)"), col_th("Share of amount"),
          col_th("Amt / claim (KES)"), col_th("Unique members")
        )
      ),
      tags$tbody(do.call(tagList, rows), national_row)
    )
  )
}

# ---- Table 3 UI: county x level cross-tab (plain, sortable) --------------------

.phc_table3_ui <- function() {
  rows <- lapply(seq_len(nrow(phc_table3)), function(i) {
    r <- phc_table3[i, ]
    tags$tr(
      tags$td(class = "fw-medium", r$county),
      lapply(.PHC_LEVELS, function(lv) tags$td(.phc_fmt_000(r[[lv]]))),
      tags$td(class = "fw-semibold", .phc_fmt_000(r$total))
    )
  })
  national_row <- tags$tr(class = "table-secondary fw-semibold",
    tags$td("National"),
    lapply(.PHC_LEVELS, function(lv) tags$td(.phc_fmt_000(sum(phc_table3[[lv]])))),
    tags$td(.phc_fmt_000(sum(phc_table3$total)))
  )
  div(class = "table-responsive", style = "max-height:520px; overflow-y:auto;",
    tags$table(class = "table table-hover table-striped align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("County", "160px"),
          lapply(.PHC_LEVELS, function(lv) col_th(gsub("Level ", "L", .PHC_LEVEL_LABELS[[lv]]))),
          col_th("Total (KES '000)")
        )
      ),
      tags$tbody(do.call(tagList, rows), national_row)
    )
  )
}

# ---- Table 4 UI: monthly allocation by facility x payment status ----------------
# All rows are shown directly - no expand/collapse required to see the
# Paid/Not paid/All breakdown for a facility.

.phc_table4_ui <- function() {
  facility_blocks <- lapply(phc_table4_facilities, function(f) {
    header <- tags$tr(class = "table-secondary",
      tags$td(colspan = 14, class = "fw-semibold",
        paste0(f$name, " — ", f$county, " — ", f$level),
        tags$span(class = "text-muted ms-2", style = "font-size:.78rem; font-weight:400;",
                   paste0("Total: ", fmt_num(f$total_all), " (KES '000)"))
      )
    )
    paid_row <- tags$tr(
      tags$td(class = "text-secondary", style = "padding-left:1.5rem;", "Paid"),
      lapply(f$months$paid, function(v) tags$td(class = "text-secondary", fmt_num(v))),
      tags$td(class = "text-secondary fw-semibold", fmt_num(f$total_paid))
    )
    notpaid_row <- tags$tr(
      tags$td(class = "text-secondary", style = "padding-left:1.5rem;", "Not paid"),
      lapply(f$months$notpaid, function(v) tags$td(class = "text-secondary", fmt_num(v))),
      tags$td(class = "text-secondary fw-semibold", fmt_num(f$total_notpaid))
    )
    all_row <- tags$tr(style = "background:var(--muted);",
      tags$td(class = "fw-semibold", style = "padding-left:1.5rem;", "All"),
      lapply(f$months$all, function(v) tags$td(class = "fw-semibold", fmt_num(v))),
      tags$td(class = "fw-semibold", fmt_num(f$total_all))
    )
    tagList(header, paid_row, notpaid_row, all_row)
  })

  national_paid_total    <- sum(phc_table4_national$paid)
  national_notpaid_total <- sum(phc_table4_national$notpaid)
  national_all_total     <- sum(phc_table4_national$all)

  national_block <- tagList(
    tags$tr(class = "table-secondary",
      tags$td(colspan = 14, class = "fw-semibold",
        "National total (all transacting facilities)")),
    tags$tr(class = "table-secondary",
      tags$td("Paid"),
      lapply(phc_table4_national$paid, function(v) tags$td(fmt_num(v))),
      tags$td(class = "fw-semibold", fmt_num(national_paid_total))),
    tags$tr(class = "table-secondary",
      tags$td("Not paid"),
      lapply(phc_table4_national$notpaid, function(v) tags$td(fmt_num(v))),
      tags$td(class = "fw-semibold", fmt_num(national_notpaid_total))),
    tags$tr(class = "table-secondary fw-semibold",
      tags$td("All"),
      lapply(phc_table4_national$all, function(v) tags$td(fmt_num(v))),
      tags$td(fmt_num(national_all_total)))
  )

  div(class = "table-responsive",
    tags$table(class = "table table-hover align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          tags$th("Facility / Status"),
          lapply(.phc_months, function(m) tags$th(m)),
          tags$th("Total (KES '000)")
        )
      ),
      tags$tbody(do.call(tagList, facility_blocks), national_block)
    )
  )
}

# ---- Tables 5-8 UI: diagnosis / intervention ranking (shared shape) -------------

.phc_ranking_table_ui <- function(ranking, code_label, show_chapter = TRUE) {
  df <- ranking$rows
  rows <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    tags$tr(
      tags$td(class = "text-muted", r$rank),
      tags$td(tags$code(r$code)),
      tags$td(class = "fw-medium", r$label),
      if (show_chapter) tags$td(class = "text-secondary", style = "font-size:.82rem;", r$chapter),
      tags$td(fmt_num(r$claims)),
      tags$td(class = "text-muted", paste0(sprintf("%.1f", r$pct), "%")),
      tags$td(fmt_num(r$unique_members)),
      tags$td(sprintf("%.2f", r$claims_per_member))
    )
  })
  subtotal_row <- tags$tr(class = "table-secondary fw-semibold",
    tags$td(colspan = if (show_chapter) 4 else 3, paste0("Top ", nrow(df), " subtotal")),
    tags$td(fmt_num(ranking$top_subtotal)),
    tags$td(paste0(sprintf("%.1f", ranking$top_subtotal / ranking$all_total * 100), "%")),
    tags$td(""), tags$td("")
  )
  all_row <- tags$tr(class = "fw-semibold",
    tags$td(colspan = if (show_chapter) 4 else 3, "All"),
    tags$td(fmt_num(ranking$all_total)), tags$td("100.0%"), tags$td(""), tags$td("")
  )
  div(class = "table-responsive",
    tags$table(class = "table table-hover align-middle mb-0",
      tags$thead(class = "table-light",
        tags$tr(
          col_th("Rank", "60px"), col_th(code_label, "110px"),
          col_th(if (code_label == "SHA Code") "Intervention name" else "Diagnosis title"),
          if (show_chapter) col_th("Chapter"),
          col_th("Claims"), col_th("%"), col_th("Unique members"), col_th("Claims / member")
        )
      ),
      tags$tbody(do.call(tagList, rows), subtotal_row, all_row)
    )
  )
}

# ---- Public: panel UI ------------------------------------------------------------

phc_panel_ui <- function() {
  s <- phc_summary

  div(class = "container-fluid px-4 py-4",

    indicator_header(
      "PHC Claims & Allocation",
      last_updated = "10 Aug 2026",
      source        = "PHC batch extract, FY2025/26",
      info = paste0(
        "Facility-level PHC claims and allocation data, reporting period 1 Jul 2025 to ",
        "30 Jun 2026. PHC maternity claims are retained throughout - not separated out ",
        "or excluded from any table below. Records with a missing level, ownership, ",
        "county, diagnosis or intervention are kept in an explicit 'not stated' / ",
        "'not classified' category rather than dropped, so every column sums to its ",
        "stated total."
      ),
      badges = tagList(
        tags$span(class = "badge text-bg-primary px-3 py-2 rounded-pill",   "DHA Leadership"),
        tags$span(class = "badge text-bg-secondary px-3 py-2 rounded-pill", "County Health Teams")
      )
    ),

    insight_banner(
      paste0(
        .PHC_LEVEL_LABELS[["l2"]], " and ", .PHC_LEVEL_LABELS[["l3"]],
        " together account for ",
        .phc_pct(sum(phc_level_totals$amount[phc_level_totals$level %in% c("l2","l3")]) /
                   phc_national_amount * 100),
        " of total PHC spend, despite being the lowest-cost tier per claim."
      ),
      type = "info"
    ),

    # Headline indicators
    tags$h6(class = "ind-section-label", "Headline Indicators"),
    div(class = "row row-cols-1 row-cols-sm-2 row-cols-xl-5 g-3",
      .phc_metric_card("Total PHC Amount", .phc_fmt_000(s$total_amount),
                       "bi bi-cash-coin", "#0284c7", sub_value = "KES '000"),
      .phc_metric_card("Transacting Facilities", fmt_num(s$total_facilities),
                       "bi bi-hospital", "#16a34a"),
      .phc_metric_card("Claims", fmt_num(s$total_claims),
                       "bi bi-file-earmark-medical-fill", "#f59e0b"),
      .phc_metric_card("Unique Members", fmt_num(s$unique_members),
                       "bi bi-people-fill", "#8b5cf6"),
      .phc_metric_card("Visit Frequency", paste0(s$visit_frequency, " / month"),
                       "bi bi-graph-up", "#ea580c",
                       sub_value = paste0(s$period_months, "-month period"))
    ),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label",
      "Table 1 — Facilities, Claims and Total Amount by Facility Level and Ownership"),
    div(class = "card border-0 shadow-sm mb-4", .phc_table1_ui()),

    tags$h6(class = "ind-section-label", "Table 2 — Facilities, Claims and Total Amount by County"),
    div(class = "card border-0 shadow-sm mb-4", .phc_table2_ui()),

    tags$h6(class = "ind-section-label", "Table 3 — Total Amount by County and Facility Level"),
    div(class = "card border-0 shadow-sm mb-4", .phc_table3_ui()),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label", "Spend Concentration"),
    div(class = "row g-4",
      div(class = "col-12 col-xl-6", .phc_chart_card("Total Amount by Facility Level", NULL, "phcLevelChart")),
      div(class = "col-12 col-xl-6", .phc_chart_card("Top 10 Counties by Total Amount", NULL, "phcCountyChart"))
    ),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label",
      "Table 4 — Month-on-Month Allocation per Facility, by Payment Status"),
    div(class = "text-muted mb-2", style = "font-size:.8rem;",
      paste0("Allocation month (Jul – Jun), not month of service — sits on a different base ",
             "to Tables 1–3. Shown for 5 representative facilities; repeats for every ",
             "transacting facility.")),
    div(class = "card border-0 shadow-sm mb-4", .phc_table4_ui()),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label", "Table 5 — Twenty Most Frequent ICD-11 Diagnoses, National"),
    div(class = "text-muted mb-2", style = "font-size:.8rem;",
      "Diagnosis codes/titles below are illustrative placeholders pending the real ICD-11 stem-code dictionary."),
    div(class = "card border-0 shadow-sm mb-4",
      .phc_ranking_table_ui(phc_diag_national, "ICD-11 Code")),

    tags$h6(class = "ind-section-label", "Table 6 — Twenty Most Frequent Diagnoses by County"),
    div(class = "text-muted mb-2", style = "font-size:.8rem;",
      "Shown for 3 representative counties; repeats for all 47 counties."),
    lapply(.PHC_DIAG_COUNTY_PANELS, function(co) {
      div(class = "card border-0 shadow-sm mb-3",
        div(class = "card-header bg-white border-bottom px-4 py-2 fw-semibold", co),
        .phc_ranking_table_ui(phc_diag_by_county[[co]], "ICD-11 Code")
      )
    }),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label", "Table 7 — Twenty Most Frequent Diagnoses by Facility"),
    div(class = "text-muted mb-2", style = "font-size:.8rem;",
      "Shown for 3 representative facilities; repeats for every transacting facility."),
    lapply(phc_diag_by_facility, function(f) {
      div(class = "card border-0 shadow-sm mb-3",
        div(class = "card-header bg-white border-bottom px-4 py-2",
          tags$span(class = "fw-semibold", f$name),
          tags$span(class = "text-muted ms-2", style = "font-size:.82rem;",
                     paste0(f$county, " · ", f$level))),
        .phc_ranking_table_ui(f$ranking, "ICD-11 Code")
      )
    }),

    tags$hr(class = "my-4 border-light"),

    tags$h6(class = "ind-section-label", "Table 8 — Twenty Most Frequent Interventions, National"),
    div(class = "text-muted mb-2", style = "font-size:.8rem;",
      "Volume-only — a claim can carry more than one intervention, so the record total exceeds the claim total."),
    div(class = "card border-0 shadow-sm",
      .phc_ranking_table_ui(phc_intervention_national, "SHA Code", show_chapter = FALSE))
  )
}

# ---- Public: server ----------------------------------------------------------------

phc_server <- function(input, output, session) {}

# ---- Data Model samples -------------------------------------------------------------

phc_raw_sample <- data.frame(
  claim_id      = c("PHC-70211","PHC-70212","PHC-70213","PHC-70214","PHC-70215"),
  facility_id   = c("FID-01-224801-3","FID-01-118934-6","FID-32-087245-2",
                    "FID-42-156723-9","FID-27-203481-5"),
  facility_name = c("Kibera Level 2 Dispensary","Mathare North Health Centre",
                    "Nakuru Level 4 Hospital","Kisumu Central Health Centre",
                    "Kilifi County Referral Hospital"),
  level         = c("Level 2","Level 3","Level 4","Level 3","Level 5"),
  ownership     = c("Public","FBO","Public","Public","Public"),
  county        = c("Nairobi City","Nairobi City","Nakuru","Kisumu","Kilifi"),
  member_id     = c("MEM-55210","MEM-55211","MEM-55212","MEM-55213","MEM-55214"),
  diagnosis_code= c("CA07.0","1F03","BA00.Z","GC30.0","JA63.0"),
  intervention_code = c("PHC-01","PHC-03","PHC-01","PHC-07","PHC-04"),
  claim_amount  = c(950,1180,1650,1120,2400),
  service_date  = c("2026-07-03","2026-07-04","2026-07-05","2026-07-05","2026-07-06"),
  stringsAsFactors = FALSE
)

phc_modelled_sample <- data.frame(
  county          = phc_table2$county[1:5],
  facilities      = phc_table2$facilities[1:5],
  claims          = phc_table2$claims[1:5],
  total_amount_KES= round(phc_table2$amount[1:5]),
  share_pct       = phc_table2$share_pct[1:5],
  amount_per_claim= round(phc_table2$amount_per_claim[1:5]),
  unique_members  = phc_table2$unique_members[1:5],
  stringsAsFactors = FALSE
)
