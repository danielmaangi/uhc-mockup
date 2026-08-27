library(shiny)
library(bslib)
library(jsonlite)

if (!tinytex::is_tinytex() && !nzchar(Sys.which("xelatex"))) {
  tinytex::install_tinytex()
}

source("R/helpers.R")
source("R/indicator_def.R")
source("R/approved_unpaid.R")
source("R/claims_tat.R")
source("R/cancer_payments.R")
source("R/claims_ageing.R")
source("R/sha_deliveries.R")
source("R/ambulance_claims.R")
source("R/phc.R")
source("R/actuarial.R")

# ==============================================================================
# NAVIGATION DEFINITION
# Each entry: id, label, icon, category, panel_ui
# Add new indicators here — no other changes needed.
# ==============================================================================

GUIDE_VERSION <- "1.0"
DEFAULT_TAB   <- "amb"

INDICATORS <- list(
  # No category = rendered as a flat top-level item, no collapsible group.
  list(id = "amb", label = "Ambulance Claims",
       icon = "bi-truck-front-fill",   category = NA),

  list(id = "phc", label = "PHC Claims & Allocation",
       icon = "bi-clipboard2-pulse-fill", category = "PHC"),
  list(id = "act", label = "SHA Plus Membership & Claims",
       icon = "bi-graph-up-arrow",        category = "Actuarial"),

  # Archived - these mockups have since been built for real, so they're kept
  # for reference but no longer the active mockup work.
  list(id = "apx", label = "Unpaid Claims in ERP",
       icon = "bi-file-earmark-check", category = "Archive"),
  list(id = "tat", label = "Claims TAT",
       icon = "bi-stopwatch",          category = "Archive"),
  list(id = "age", label = "Claims Ageing Report",
       icon = "bi-hourglass-split",    category = "Archive"),
  list(id = "cnc", label = "Cancer Registry",
       icon = "bi-heart-pulse-fill",   category = "Archive"),
  list(id = "dlv", label = "Deliveries financed",
       icon = "bi-hospital",           category = "Archive"),
  list(id = "cur", label = "Claims Under Review",
       icon = "bi-hourglass",          category = "Archive")
)

# ==============================================================================
# SIDEBAR NAVIGATION
# ==============================================================================

.nav_item <- function(ind, is_first) {
  div(
    id    = paste0("nav-", ind$id),
    class = paste("sidebar-nav-item", if (is_first) "active" else ""),
    `data-label` = tolower(ind$label),
    onclick = sprintf(
      "Shiny.setInputValue('nav_click', '%s', {priority: 'event'})",
      ind$id
    ),
    span(ind$label)
  )
}

.build_sidebar_nav <- function(indicators) {
  # Indicators with no category render as flat top-level items - no
  # collapsible group, no numeric prefix.
  ungrouped <- Filter(function(x) is.na(x$category), indicators)
  grouped   <- Filter(function(x) !is.na(x$category), indicators)

  flat_items <- lapply(seq_along(ungrouped), function(i) {
    .nav_item(ungrouped[[i]], i == 1)
  })

  categories <- unique(sapply(grouped, `[[`, "category"))
  groups <- lapply(seq_along(categories), function(ci) {
    cat    <- categories[[ci]]
    items  <- Filter(function(x) x$category == cat, grouped)
    grp_id <- gsub("[^a-z0-9]", "-", tolower(cat))
    div(class = "sidebar-cat-group",
      div(class = "sidebar-category-label",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#cat-", grp_id),
        `aria-expanded`  = "false",
        style = "cursor:pointer;",
        tags$i(class = paste("bi me-2", if (cat == "Archive") "bi-archive" else "bi-collection")),
        span(class = "flex-grow-1", paste0(ci, ". ", cat)),
        tags$i(class = "bi bi-chevron-down sidebar-chevron")
      ),
      div(id    = paste0("cat-", grp_id),
          class = "collapse sidebar-cat-items",
        lapply(seq_along(items), function(i) {
          ind <- items[[i]]
          ind$label <- paste0(ci, ".", i, " ", ind$label)
          .nav_item(ind, length(ungrouped) == 0 && ci == 1 && i == 1)
        })
      )
    )
  })
  do.call(tagList, c(flat_items, groups))
}

app_sidebar <- sidebar(
  width  = 260,
  bg     = "#f1f5f9",
  class  = "app-sidebar",

  # Search box
  div(class = "px-2 mb-1",
    tags$input(
      type = "text", id = "sidebar_search",
      class = "form-control form-control-sm sidebar-search",
      placeholder = "Search indicators..."
    )
  ),

  # Indicator navigation
  div(class = "sidebar-nav-list",
    .build_sidebar_nav(INDICATORS)
  ),

  # Export reference guide
  div(class = "px-3 mb-2",
    downloadButton(
      "dl_reference_guide",
      label = paste0("Reference Guide v", GUIDE_VERSION),
      class = "btn btn-sm w-100 sidebar-guide-btn"
    )
  ),

  # Dummy data disclaimer
  div(class = "sidebar-disclaimer",
    tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
    "Demo only — all data is fictional and does not represent real claims or patients."
  )
)

# ==============================================================================
# STYLES & SCRIPTS
# ==============================================================================

app_css <- HTML("
  /* ── Design tokens (ported from main application stylesheet) ────────── */
  :root {
    --radius: 0.65rem;
    --background:             oklch(1 0 0);
    --foreground:             oklch(0.2 0.02 260);
    --card:                   oklch(1 0 0);
    --card-foreground:        oklch(0.2 0.02 260);
    --primary:                oklch(0.55 0.16 232);
    --primary-foreground:     oklch(0.99 0.01 232);
    --secondary:              oklch(0.96 0.004 260);
    --secondary-foreground:   oklch(0.25 0.02 260);
    --muted:                  oklch(0.965 0.004 260);
    --muted-foreground:       oklch(0.45 0.02 260);
    --accent:                 oklch(0.965 0.004 260);
    --accent-foreground:      oklch(0.25 0.02 260);
    --border:                 oklch(0.9 0.006 260);
    --input:                  oklch(0.9 0.006 260);
    --ring:                   oklch(0.55 0.16 232);
    --sidebar:                #f1f5f9;
    --sidebar-foreground:     oklch(0.2 0.02 260);
    --sidebar-primary:        oklch(0.55 0.16 232);
    --sidebar-primary-fg:     oklch(0.99 0.01 232);
    --sidebar-accent:         oklch(0.965 0.004 260);
    --sidebar-accent-fg:      oklch(0.25 0.02 260);
    --sidebar-border:         oklch(0.9 0.006 260);
  }

  /* ── Fonts ──────────────────────────────────────────────────────────── */
  @import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap');
  body {
    font-family: 'Plus Jakarta Sans', system-ui, sans-serif;
    background: oklch(0.97 0.003 260);
    color: var(--foreground);
    -webkit-font-smoothing: antialiased;
  }

  /* ── Navbar ─────────────────────────────────────────────────────────── */
  .navbar {
    background-color: #1e293b !important;
    border-bottom: 1px solid #0f172a !important;
  }
  .navbar .navbar-brand,
  .navbar-brand span,
  .bslib-page-title {
    color: #f1f5f9 !important;
    font-weight: 600;
  }

  /* ── Sidebar ─────────────────────────────────────────────────────────── */
  .app-sidebar { border-right: 1px solid var(--sidebar-border) !important; }

  .sidebar-brand {
    display: flex; align-items: center; gap: .75rem;
    padding: 1.2rem 1rem 1rem;
    border-bottom: 1px solid var(--sidebar-border);
    margin-bottom: .5rem;
  }
  .sidebar-brand-icon  { font-size: 1.45rem; color: var(--primary); }
  .sidebar-brand-name  { font-size: .9rem; font-weight: 700;
                         color: var(--sidebar-foreground); line-height: 1.2; }
  .sidebar-brand-sub   { font-size: .62rem; color: var(--muted-foreground);
                         text-transform: uppercase; letter-spacing: .08em; }

  .sidebar-divider { border-color: var(--sidebar-border); margin: .25rem .75rem .5rem; }

  /* Search input */
  .sidebar-search {
    font-size: .8rem !important; border-radius: .5rem !important;
    border-color: var(--sidebar-border) !important;
    background: #e8edf3 !important;
    padding: .32rem .65rem !important;
  }
  .sidebar-search:focus {
    border-color: var(--primary) !important;
    box-shadow: 0 0 0 2px oklch(0.55 0.16 232 / 0.2) !important;
    background: white !important; outline: none;
  }

  /* Category group */
  .sidebar-cat-group { margin-bottom: .15rem; }
  .sidebar-category-label {
    font-size: .62rem; font-weight: 700; color: var(--muted-foreground);
    text-transform: uppercase; letter-spacing: .1em;
    padding: .85rem 1rem .35rem; display: flex; align-items: center;
    user-select: none;
  }
  .sidebar-category-label:hover { color: var(--sidebar-foreground); }
  .sidebar-chevron {
    font-size: .65rem; transition: transform .2s ease; margin-left: .25rem;
  }
  .sidebar-category-label.collapsed .sidebar-chevron { transform: rotate(-90deg); }

  /* Nav items */
  .sidebar-nav-item {
    display: flex; align-items: center; gap: .6rem;
    padding: .48rem .85rem;
    font-size: .835rem; font-weight: 500;
    color: var(--sidebar-foreground); opacity: .7;
    border-radius: calc(var(--radius) - 2px);
    margin: .1rem .5rem;
    border-left: 3px solid transparent;
    cursor: pointer;
    transition: background .15s, color .15s, opacity .15s, border-color .15s;
  }
  .sidebar-nav-item:hover {
    background: var(--sidebar-accent);
    color: var(--sidebar-accent-fg);
    opacity: 1;
  }
  .sidebar-nav-item.active {
    border-left-color: var(--primary);
    background: oklch(0.55 0.16 232 / 0.1);
    color: var(--primary);
    opacity: 1; font-weight: 600;
  }
  .sidebar-nav-item.active .sidebar-nav-icon { opacity: .9; }
  .sidebar-nav-icon { font-size: .9rem; flex-shrink: 0; }

  /* ── Reference guide export button ─────────────────────────────────── */
  .sidebar-guide-btn {
    font-size: .74rem !important; font-weight: 600 !important;
    color: var(--primary) !important;
    background: oklch(0.95 0.025 232) !important;
    border: 1px solid oklch(0.86 0.04 232) !important;
    border-radius: .45rem !important;
    transition: background .15s, color .15s;
  }
  .sidebar-guide-btn:hover {
    background: var(--primary) !important;
    color: white !important;
    border-color: var(--primary) !important;
  }

  /* ── Dummy data disclaimer ──────────────────────────────────────────── */
  .sidebar-disclaimer {
    margin: auto 0.75rem 0.75rem;
    padding: 0.5rem 0.65rem;
    font-size: .68rem; line-height: 1.45;
    color: #92400e;
    background: #fef3c7;
    border: 1px solid #fde68a;
    border-radius: 0.45rem;
  }

  /* ── Cards ───────────────────────────────────────────────────────────── */
  .bslib-sidebar-layout > .main { padding: 0 !important; }
  .card { border-radius: var(--radius) !important; border-color: var(--border) !important; }
  .card-header, .card-footer { border-color: var(--border) !important; }

  /* ── Standard table theme ────────────────────────────────────────────── */
  .table {
    font-size: .865rem;
    --bs-table-hover-bg:    var(--accent);
    --bs-table-border-color: var(--border);
    margin-bottom: 0;
  }
  /* Unified header: small-caps labels, muted on subtle bg */
  .table thead th,
  .table-light th {
    font-size: .7rem !important;
    font-weight: 700 !important;
    text-transform: uppercase;
    letter-spacing: .055em;
    color: var(--muted-foreground) !important;
    background: var(--muted) !important;
    border-bottom: 1px solid var(--border) !important;
    padding: .65rem 1rem !important;
    white-space: nowrap;
  }
  .table tbody td {
    padding: .7rem 1rem !important;
    border-color: var(--border) !important;
    color: var(--foreground);
    vertical-align: middle;
  }
  .table tbody tr:last-child td { border-bottom: none; }
  /* Sortable column hover */
  .sortable-col       { cursor: pointer; user-select: none; white-space: nowrap; }
  .sortable-col:hover { background: var(--accent) !important; }

  /* ── Indicator section labels ───────────────────────────────────────── */
  .ind-section-label {
    font-size: .67rem; font-weight: 700; text-transform: uppercase;
    letter-spacing: .09em; color: var(--muted-foreground); margin-bottom: .75rem;
  }

  /* ── Filter bar ─────────────────────────────────────────────────────── */
  .filter-card {
    background: linear-gradient(135deg, oklch(0.987 0.007 232), oklch(0.975 0.003 260)) !important;
    border-left: 3px solid var(--primary) !important;
  }
  .filter-section-header {
    display: flex; align-items: center; gap: .45rem;
    font-size: .69rem; font-weight: 700; text-transform: uppercase;
    letter-spacing: .1em; color: var(--primary);
    padding-bottom: .65rem; margin-bottom: .1rem;
    border-bottom: 1px solid oklch(0.9 0.016 232);
  }
  .filter-bar-label {
    font-size: .67rem; font-weight: 700; color: var(--muted-foreground);
    text-transform: uppercase; letter-spacing: .06em;
    margin-bottom: .28rem; display: flex; align-items: center; gap: .25rem;
  }
  /* Inputs */
  .filter-bar-group .form-select,
  .filter-bar-group .form-control {
    font-size: .82rem !important;
    border-radius: .5rem !important;
    border-color: oklch(0.87 0.014 232) !important;
    background: white !important;
    padding: .35rem .65rem !important;
    transition: border-color .15s, box-shadow .15s;
  }
  .filter-bar-group .form-select:focus,
  .filter-bar-group .form-control:focus {
    border-color: var(--primary) !important;
    box-shadow: 0 0 0 3px oklch(0.55 0.16 232 / 0.14) !important;
    outline: none;
  }
  /* Selectize overrides */
  .filter-bar-group .selectize-control { border-radius: .5rem; }
  .filter-bar-group .selectize-input {
    font-size: .82rem !important;
    border-color: oklch(0.87 0.014 232) !important;
    border-radius: .5rem !important;
    background: white !important;
    padding: .32rem .65rem !important;
    box-shadow: none !important;
    min-height: 34px !important;
    line-height: 1.5 !important;
  }
  .filter-bar-group .selectize-input.focus {
    border-color: var(--primary) !important;
    box-shadow: 0 0 0 3px oklch(0.55 0.16 232 / 0.14) !important;
  }
  .filter-bar-group .selectize-input > .item {
    background: oklch(0.92 0.028 232) !important;
    color: oklch(0.42 0.13 232) !important;
    border: 1px solid oklch(0.83 0.038 232) !important;
    border-radius: .35rem !important;
    font-size: .73rem !important; font-weight: 600 !important;
    padding: .06rem .45rem !important;
    margin: .1rem .15rem .1rem 0 !important;
  }
  /* Filter action buttons */
  .btn-filter-apply {
    font-size: .82rem; font-weight: 600;
    padding: .42rem 1.1rem; border-radius: .5rem; letter-spacing: .01em;
  }
  .btn-filter-reset {
    font-size: .82rem; padding: .42rem .7rem;
    border-radius: .5rem;
    color: var(--muted-foreground) !important;
    border-color: oklch(0.87 0.014 232) !important;
    background: white !important;
  }
  .btn-filter-reset:hover {
    background: var(--accent) !important;
    border-color: var(--border) !important;
  }
  /* APX county search */
  .apx-search-wrap .input-group-text {
    border-color: oklch(0.87 0.014 232) !important;
    border-radius: .5rem 0 0 .5rem !important;
    background: white !important;
    padding: .3rem .55rem !important;
  }
  .apx-search-wrap .apx-search {
    border-color: oklch(0.87 0.014 232) !important;
    border-radius: 0 .5rem .5rem 0 !important;
    font-size: .82rem !important;
    padding: .3rem .55rem !important;
    transition: border-color .15s, box-shadow .15s;
  }
  .apx-search-wrap .apx-search:focus {
    border-color: var(--primary) !important;
    box-shadow: 0 0 0 3px oklch(0.55 0.16 232 / 0.14) !important;
    outline: none;
  }

  /* ── Fund-type pills (Approved vs Unpaid) ───────────────────────────── */
  .nav-pills .nav-link {
    font-weight: 500; color: var(--muted-foreground);
    border-radius: var(--radius); padding: .38rem 1rem;
  }
  .nav-pills .nav-link.active {
    background: var(--primary); color: var(--primary-foreground);
  }

  /* ── Alternating row colors ─────────────────────────────────────────── */
  /* Plain tables (TAT, Cancer): Bootstrap stripe color */
  .table-striped { --bs-table-striped-bg: oklch(0.955 0.012 232); }
  /* APX county rows: explicit class on every other row */
  .county-row-alt > td { background: oklch(0.955 0.012 232) !important; }

  /* ── Collapsible county rows (Approved vs Unpaid) ───────────────────── */
  .county-row          { cursor: pointer; user-select: none; }
  .county-row:hover td { background: var(--accent) !important; }
  .county-row.expanded td {
    background: oklch(0.96 0.012 232) !important;
  }
  .county-row.expanded .expand-chevron {
    transform: rotate(90deg); color: var(--primary) !important;
  }
  .expand-chevron     { transition: transform .18s ease; display: inline-block; }
  .county-name-cell   { padding-left: 1rem !important; }
  .provider-name-cell { padding-left: 2.8rem !important; }
  .provider-row td    { background: var(--muted); border-top: 1px solid var(--border); }
")

app_js <- HTML("
  /* ── Mock action feedback (export / escalate buttons) ───────────────── */
  window.showMockAction = function(msg, type) {
    var bg = (type === 'danger') ? 'text-bg-danger' : 'text-bg-success';
    var el = document.createElement('div');
    el.className = 'toast align-items-center ' + bg + ' border-0 position-fixed';
    el.style.cssText = 'bottom:1.25rem;right:1.25rem;z-index:9999;min-width:290px;';
    el.setAttribute('role', 'alert');
    el.innerHTML =
      '<div class=\"d-flex\"><div class=\"toast-body fw-medium\" style=\"font-size:.875rem;\">' +
      msg + '</div>' +
      '<button type=\"button\" class=\"btn-close btn-close-white me-2 m-auto\" data-bs-dismiss=\"toast\"></button></div>';
    document.body.appendChild(el);
    var t = new bootstrap.Toast(el, { delay: 3500 });
    t.show();
    el.addEventListener('hidden.bs.toast', function() { el.remove(); });
  };

  /* ── Bootstrap tooltips ─────────────────────────────────────────────── */
  document.addEventListener('DOMContentLoaded', function() {
    document.querySelectorAll('[data-bs-toggle=\"tooltip\"]').forEach(function(el) {
      new bootstrap.Tooltip(el, { html: true });
    });
  });

  /* ── Nav click: switch indicator panel ─────────────────────────────── */
  Shiny.addCustomMessageHandler('setActiveNav', function(id) {
    $('.sidebar-nav-item').removeClass('active');
    $('#nav-' + id).addClass('active');
    /* Initialise Chart.js charts when panel first becomes visible */
    if (id === 'apx' && typeof window.revealApxCharts === 'function') {
      setTimeout(window.revealApxCharts, 80);
    }
    if (id === 'tat' && typeof window.revealTatCharts === 'function') {
      setTimeout(window.revealTatCharts, 80);
    }
    if (id === 'cnc' && typeof window.revealCancerCharts === 'function') {
      setTimeout(window.revealCancerCharts, 80);
    }
    if (id === 'age' && typeof window.revealAgeingCharts === 'function') {
      setTimeout(window.revealAgeingCharts, 80);
    }
    if (id === 'dlv' && typeof window.revealDeliveriesCharts === 'function') {
      setTimeout(window.revealDeliveriesCharts, 80);
    }
    if (id === 'amb' && typeof window.revealAmbCharts === 'function') {
      setTimeout(window.revealAmbCharts, 80);
    }
    if (id === 'phc' && typeof window.revealPhcCharts === 'function') {
      setTimeout(window.revealPhcCharts, 80);
    }
    if (id === 'act' && typeof window.revealActCharts === 'function') {
      setTimeout(window.revealActCharts, 80);
    }
  });

  /* ── Sidebar search filter ──────────────────────────────────────────── */
  document.addEventListener('DOMContentLoaded', function() {
    var searchEl = document.getElementById('sidebar_search');
    if (!searchEl) return;
    searchEl.addEventListener('input', function() {
      var q = this.value.toLowerCase().trim();
      document.querySelectorAll('.sidebar-nav-item').forEach(function(item) {
        var match = !q || (item.dataset.label || '').includes(q);
        item.style.display = match ? '' : 'none';
      });
      document.querySelectorAll('.sidebar-cat-group').forEach(function(grp) {
        var hasVisible = Array.from(grp.querySelectorAll('.sidebar-nav-item'))
          .some(function(i) { return i.style.display !== 'none'; });
        grp.style.display = hasVisible ? '' : 'none';
        if (hasVisible && q) {
          var catItems = grp.querySelector('.sidebar-cat-items');
          if (catItems) catItems.classList.add('show');
        }
      });
    });
  });

  /* ── County expand / collapse (Approved vs Unpaid) ─────────────────── */
  $(document).on('click', '.county-row', function() {
    var cid  = $(this).data('county-id');
    var rows = $('.' + cid);
    if (rows.first().is(':visible')) {
      rows.hide(); $(this).removeClass('expanded');
    } else {
      rows.show(); $(this).addClass('expanded');
    }
  });

  /* ── County search (Approved vs Unpaid) ─────────────────────────────── */
  $(document).on('input', '.apx-search', function() {
    var q    = $(this).val().toLowerCase();
    var fund = $(this).data('fund');
    $('table[data-fund=\"' + fund + '\"]').find('.county-row').each(function() {
      var name = $(this).find('.county-name-cell').text().trim().toLowerCase();
      var cid  = $(this).data('county-id');
      if (name.indexOf(q) !== -1) {
        $(this).show();
      } else {
        $(this).hide().removeClass('expanded');
        $('.' + cid).hide();
      }
    });
  });

  /* ── Sortable columns (all tables) ──────────────────────────────────── */
  $(document).on('click', '.sortable-col', function() {
    var th    = $(this);
    var table = th.closest('table');
    var tbody = table.find('tbody');
    var idx   = th.index();
    var asc   = th.hasClass('sort-asc');
    table.find('.sortable-col').removeClass('sort-asc sort-desc');
    th.addClass(asc ? 'sort-desc' : 'sort-asc');

    if (tbody.find('.county-row').length > 0) {
      /* Approved vs Unpaid: sort county rows, provider rows follow */
      var countyRows = tbody.find('.county-row').toArray();
      countyRows.sort(function(a, b) {
        var aT = $(a).find('td').eq(idx).text().trim();
        var bT = $(b).find('td').eq(idx).text().trim();
        var aN = parseFloat(aT.replace(/[^0-9.]/g, ''));
        var bN = parseFloat(bT.replace(/[^0-9.]/g, ''));
        if (!isNaN(aN) && !isNaN(bN)) return asc ? bN - aN : aN - bN;
        return asc ? bT.localeCompare(aT) : aT.localeCompare(bT);
      });
      countyRows.forEach(function(row) {
        var cid = $(row).data('county-id');
        tbody.append(row);
        tbody.find('.' + cid).each(function() { tbody.append(this); });
      });
    } else {
      /* TAT county table: plain sort */
      var rows = tbody.find('tr').toArray();
      rows.sort(function(a, b) {
        var aT = $(a).find('td').eq(idx).text().trim();
        var bT = $(b).find('td').eq(idx).text().trim();
        var aN = parseFloat(aT.replace(/[^0-9.]/g, ''));
        var bN = parseFloat(bT.replace(/[^0-9.]/g, ''));
        if (!isNaN(aN) && !isNaN(bN)) return asc ? bN - aN : aN - bN;
        return asc ? bT.localeCompare(aT) : aT.localeCompare(bT);
      });
      rows.forEach(function(r) { tbody.append(r); });
    }
  });
")

# ==============================================================================
# UI
# ==============================================================================

ui <- page_sidebar(
  title   = "Ambulance Claims",
  theme   = bs_theme(version = 5, primary = "#0e82b5"),   # ≈ oklch(0.55 0.16 232)
  sidebar = app_sidebar,
  fillable = FALSE,

  tags$head(
    tags$link(rel  = "stylesheet",
              href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/chart.js@4.4.3/dist/chart.umd.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/chartjs-plugin-annotation@3.1.0/dist/chartjs-plugin-annotation.min.js"),
    tags$style(app_css),
    tags$style(HTML(def_panel_css)),
    tags$script(HTML(apx_chart_js)),      # from approved_unpaid.R
    tags$script(HTML(tat_chart_js)),      # from claims_tat.R
    tags$script(HTML(cancer_chart_js)),  # from cancer_payments.R
    tags$script(HTML(ageing_chart_js)),      # from claims_ageing.R
    tags$script(HTML(deliveries_chart_js)), # from sha_deliveries.R
    tags$script(HTML(amb_chart_js)),        # from ambulance_claims.R
    tags$script(HTML(phc_chart_js)),        # from phc.R
    tags$script(HTML(act_chart_js)),        # from actuarial.R
    tags$script(app_js)
  ),

  # Hidden tabset — one tab per indicator
  tabsetPanel(
    id       = "current_indicator",
    type     = "hidden",
    selected = DEFAULT_TAB,
    tabPanel("apx", indicator_tabs_ui("apx", apx_panel_ui(),
               raw_df        = apx_raw_sample,
               modelled_df   = apx_modelled_sample,
               raw_grain     = "One row = one claim line submitted in ERP",
               modelled_grain= "One row = one county × fund, current snapshot")),
    tabPanel("tat", indicator_tabs_ui("tat", tat_panel_ui(),
               raw_df        = tat_raw_sample,
               modelled_df   = tat_modelled_sample,
               raw_grain     = "One row = one paid claim (joined: ERP payments × Payor System claim creation dates)",
               modelled_grain= "One row = one county, aggregated TAT statistics for the selected period")),
    tabPanel("age", indicator_tabs_ui("age", ageing_panel_ui(),
               raw_df        = ageing_raw_sample,
               modelled_df   = ageing_modelled_sample,
               raw_grain     = "One row = one unpaid/pending claim line",
               modelled_grain= "One row = one county, claim counts and values bucketed by days pending")),
    tabPanel("cnc", indicator_tabs_ui("cnc", cancer_panel_ui(),
               raw_df        = cancer_raw_sample,
               modelled_df   = cancer_modelled_sample,
               raw_grain     = "One row = one chemotherapy encounter (joined: Claims × providers × ICD-11 codes)",
               modelled_grain= "One row = one county, aggregated patient counts and payment totals")),
    tabPanel("dlv", indicator_tabs_ui("dlv", deliveries_panel_ui(),
               raw_df        = dlv_raw_sample,
               modelled_df   = dlv_modelled_sample,
               raw_grain     = "One row = one delivery record (joined: KHIS facility deliveries × SHA maternity claims)",
               modelled_grain= "One row = one county, total KHIS deliveries vs SHA-claimed for the selected month")),
    tabPanel("amb", indicator_tabs_ui("amb", amb_panel_ui(),
               raw_df        = amb_raw_sample,
               modelled_df   = amb_modelled_sample,
               raw_grain     = "One row = one ambulance claim (joined: Payor System submission/status/vendor/patient/county x ERP payment confirmation)",
               modelled_grain= "One row = one vendor, aggregated claim counts/values by status, rejection rates and average TAT")),
    tabPanel("phc", indicator_tabs_ui("phc", phc_panel_ui(),
               raw_df        = phc_raw_sample,
               modelled_df   = phc_modelled_sample,
               raw_grain     = "One row = one PHC claim (facility × member × diagnosis × intervention)",
               modelled_grain= "One row = one county, aggregated facility/claim counts and total amount")),
    tabPanel("act", indicator_tabs_ui("act", act_panel_ui(),
               raw_df        = act_raw_sample,
               modelled_df   = act_modelled_sample,
               raw_grain     = "One row = one contributor-month (joined: member_contribution_combined × household_dependent_combined × util, on cr_id)",
               modelled_grain= "One row = one sector × age band, aggregated contributors/contributions/claims for the schedule period")),
    tabPanel("cur", indicator_def_only_ui("cur"))
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  .valid_tabs <- sapply(INDICATORS, `[[`, "id")

  # On load: restore tab from ?tab= query parameter, falling back to the
  # default tab. Always fires setActiveNav so the landing tab's charts get
  # revealed - without this, a plain page load (no ?tab= in the URL) never
  # triggers chart initialization for whichever tab is selected by default.
  observe({
    query <- parseQueryString(session$clientData$url_search)
    tab   <- query[["tab"]]
    if (is.null(tab) || !(tab %in% .valid_tabs)) tab <- DEFAULT_TAB
    updateTabsetPanel(session, "current_indicator", selected = tab)
    session$sendCustomMessage("setActiveNav", tab)
  })

  # On nav click: switch panel and push ?tab= to the URL
  observeEvent(input$nav_click, {
    updateTabsetPanel(session, "current_indicator", selected = input$nav_click)
    session$sendCustomMessage("setActiveNav", input$nav_click)
    updateQueryString(paste0("?tab=", input$nav_click), mode = "replace")
  })

  # Delegate to each indicator's server function
  apx_server(input, output, session)
  tat_server(input, output, session)
  ageing_server(input, output, session)
  cancer_server(input, output, session)
  deliveries_server(input, output, session)
  amb_server(input, output, session)
  phc_server(input, output, session)
  act_server(input, output, session)
  def_download_server(input, output, session, sapply(INDICATORS, `[[`, "id"))

  output$dl_reference_guide <- downloadHandler(
    filename = function() paste0("UHC_Indicator_Reference_Guide_v", GUIDE_VERSION, ".pdf"),
    content  = function(file) {
      def_dir  <- normalizePath("definitions")
      template <- file.path(def_dir, "reference_guide_template.qmd")
      tmp_dir  <- tempfile()
      dir.create(tmp_dir)
      tmp_tmpl <- file.path(tmp_dir, "reference_guide_template.qmd")
      tmpl_lines <- readLines(template, encoding = "UTF-8")
      tmpl_lines <- gsub("VERSION_PLACEHOLDER", GUIDE_VERSION, tmpl_lines, fixed = TRUE)
      writeLines(tmpl_lines, tmp_tmpl)
      out_name <- paste0("UHC_Reference_Guide_v", GUIDE_VERSION, ".pdf")
      quarto::quarto_render(
        tmp_tmpl,
        execute_params = list(
          ids        = sapply(INDICATORS, `[[`, "id"),
          # Ungrouped indicators (category NA) get their own section, titled
          # with their own name, rather than a shared category heading.
          categories = sapply(INDICATORS, function(x) if (is.na(x$category)) x$label else x$category),
          def_dir    = def_dir,
          version    = GUIDE_VERSION
        ),
        output_file = out_name,
        quiet       = TRUE
      )
      file.copy(file.path(tmp_dir, out_name), file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
