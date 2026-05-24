# ==============================================================================
# Indicator definition tab
# Reads definitions/<id>.qmd — YAML frontmatter parsed, each field value
# rendered from Markdown to HTML via commonmark.
# ==============================================================================

.DEF_FIELDS <- list(
  list(key = "definition",          label = "Definition"),
  list(key = "numerator",           label = "Numerator"),
  list(key = "denominator",         label = "Denominator"),
  list(key = "unit_of_measure",     label = "Unit of Measure"),
  list(key = "data_source",         label = "Data Source"),
  list(key = "frequency",           label = "Frequency"),
  list(key = "disaggregation",      label = "Disaggregation"),
  list(key = "filters",             label = "Filters"),
  list(key = "acceptance_criteria", label = "Acceptance Criteria"),
  list(key = "notes_caveats",       label = "Data Quality Checks")
)

# Parse YAML frontmatter from a .qmd file (ignores body below second ---)
.parse_qmd_front <- function(path) {
  if (!file.exists(path)) return(NULL)
  lines  <- readLines(path, warn = FALSE, encoding = "UTF-8")
  delims <- which(trimws(lines) == "---")
  if (length(delims) < 2) return(NULL)
  block  <- lines[(delims[1] + 1):(delims[2] - 1)]
  yaml::yaml.load(paste(block, collapse = "\n"))
}

# Render a markdown string to an HTML fragment (returns shiny HTML object)
.md_html <- function(x) {
  if (is.null(x) || !nzchar(trimws(as.character(x)))) return(NULL)
  HTML(commonmark::markdown_html(as.character(x), smart = TRUE, extensions = TRUE))
}

# Cache parsed definitions at startup so files aren't re-read per render
.def_cache <- local({
  cache <- list()
  function(id) {
    if (is.null(cache[[id]])) {
      path <- file.path("definitions", paste0(id, ".qmd"))
      cache[[id]] <<- .parse_qmd_front(path)
    }
    cache[[id]]
  }
})

# Build the definition panel UI for a given indicator id
def_panel_ui <- function(id) {
  meta <- .def_cache(id)
  path <- file.path("definitions", paste0(id, ".qmd"))

  if (is.null(meta)) {
    return(
      div(class = "ind-def-empty p-4 text-center text-muted",
        tags$i(class = "bi bi-file-earmark-text fs-3 d-block mb-2"),
        "No definition file found at ",
        tags$code(path)
      )
    )
  }

  field_rows <- Filter(Negate(is.null), lapply(.DEF_FIELDS, function(f) {
    html <- .md_html(meta[[f$key]])
    if (is.null(html)) return(NULL)
    div(class = "ind-def-row",
      div(class = "ind-def-label", f$label),
      div(class = "ind-def-value", html)
    )
  }))

  div(class = "ind-def-panel",
    div(class = "ind-def-header",
      if (!is.null(meta$indicator_code))
        tags$span(class = "ind-def-code", meta$indicator_code),
      div(class = "ind-def-name",
        if (!is.null(meta$indicator_name)) meta$indicator_name else id
      ),
      downloadButton(
        paste0("dl_def_", id),
        label = "PDF",
        class = "btn btn-sm btn-outline-secondary ms-auto ind-def-dl-btn"
      )
    ),
    div(class = "ind-def-fields", do.call(tagList, field_rows))
  )
}

# Wrap a mockup UI in a Definition | Mockup tab pair
indicator_tabs_ui <- function(id, mockup_ui) {
  div(class = "ind-view-tabs",
    navset_tab(
      id = paste0("ind_view_", id),
      nav_panel(
        title = tagList(tags$i(class = "bi bi-bar-chart-line me-1"), "Mockup"),
        value = "mockup",
        mockup_ui
      ),
      nav_panel(
        title = tagList(tags$i(class = "bi bi-book me-1"), "Definition"),
        value = "def",
        div(class = "p-4", def_panel_ui(id))
      )
    )
  )
}

# Definition-only panel (no Mockup tab) — used when no mockup has been built yet
indicator_def_only_ui <- function(id) {
  div(class = "ind-view-tabs",
    navset_tab(
      id = paste0("ind_view_", id),
      nav_panel(
        title = tagList(tags$i(class = "bi bi-book me-1"), "Definition"),
        value = "def",
        div(class = "p-4", def_panel_ui(id))
      )
    )
  )
}

# Register downloadHandlers for all indicator definition PDFs
def_download_server <- function(input, output, session, ids) {
  def_dir  <- normalizePath("definitions")
  template <- file.path(def_dir, "pdf_template.qmd")
  lapply(ids, function(id) {
    local({
      .id <- id
      output[[paste0("dl_def_", .id)]] <- downloadHandler(
        filename = function() {
          meta <- .def_cache(.id)
          nm   <- if (!is.null(meta$indicator_name))
            gsub("[^a-zA-Z0-9_-]", "_", meta$indicator_name)
          else .id
          paste0(nm, "_definition.pdf")
        },
        content = function(file) {
          tmp_dir  <- tempfile()
          dir.create(tmp_dir)
          tmp_tmpl <- file.path(tmp_dir, "pdf_template.qmd")
          file.copy(template, tmp_tmpl)
          out_name <- paste0(.id, "_definition.pdf")
          quarto::quarto_render(
            tmp_tmpl,
            execute_params = list(id = .id, def_dir = def_dir),
            output_file    = out_name,
            quiet          = TRUE
          )
          file.copy(file.path(tmp_dir, out_name), file, overwrite = TRUE)
        }
      )
    })
  })
}

# CSS for the definition panel (injected via tags$style(HTML(def_panel_css)) in app.R)
def_panel_css <- "

  /* ── Definition / Mockup tab strip ──────────────────────────────────── */
  .ind-view-tabs .nav-tabs {
    padding: .5rem 1.25rem 0;
    background: white;
    border-bottom: 1px solid var(--border);
    margin-bottom: 0 !important;
  }
  .ind-view-tabs .nav-tabs .nav-link {
    font-size: .8rem; font-weight: 600;
    color: var(--muted-foreground);
    border-radius: .45rem .45rem 0 0;
    padding: .45rem .9rem;
    border: 1px solid transparent;
    transition: color .12s, background .12s;
  }
  .ind-view-tabs .nav-tabs .nav-link:hover {
    color: var(--foreground);
    background: var(--muted);
  }
  .ind-view-tabs .nav-tabs .nav-link.active {
    color: var(--primary);
    border-color: var(--border) var(--border) white;
    background: white;
    font-weight: 700;
  }
  .ind-view-tabs .tab-content { background: white; }

  /* ── Definition panel container ─────────────────────────────────────── */
  .ind-def-panel {
    max-width: 860px;
  }

  /* Header: code badge + indicator name */
  .ind-def-header {
    display: flex; align-items: center; gap: .65rem;
    margin-bottom: 1.5rem;
    padding-bottom: 1rem;
    border-bottom: 2px solid var(--border);
  }
  .ind-def-code {
    display: inline-block;
    font-size: .68rem; font-weight: 700;
    letter-spacing: .07em; text-transform: uppercase;
    color: var(--primary);
    background: oklch(0.95 0.025 232);
    border: 1px solid oklch(0.86 0.04 232);
    border-radius: .35rem;
    padding: .2rem .55rem;
    flex-shrink: 0;
  }
  .ind-def-name {
    font-size: 1.15rem; font-weight: 700;
    color: var(--foreground); line-height: 1.3;
  }

  /* Field rows */
  .ind-def-fields {
    display: flex; flex-direction: column; gap: 0;
  }
  .ind-def-row {
    display: grid;
    grid-template-columns: 160px 1fr;
    gap: .5rem 1.25rem;
    padding: .85rem 0;
    border-bottom: 1px solid var(--border);
    align-items: start;
  }
  .ind-def-row:last-child { border-bottom: none; }

  .ind-def-label {
    font-size: .72rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: .07em;
    color: var(--muted-foreground);
    padding-top: .15rem;
  }
  .ind-def-value {
    font-size: .875rem; line-height: 1.65;
    color: var(--foreground);
  }

  /* Markdown rendered inside field values */
  .ind-def-value p  { margin: 0 0 .5rem; }
  .ind-def-value p:last-child { margin-bottom: 0; }
  .ind-def-value ul, .ind-def-value ol {
    margin: .25rem 0 .5rem; padding-left: 1.35rem;
  }
  .ind-def-value li { margin-bottom: .2rem; }
  .ind-def-value strong { color: var(--foreground); font-weight: 600; }
  .ind-def-value code {
    font-size: .8rem;
    background: var(--muted);
    border: 1px solid var(--border);
    border-radius: .25rem;
    padding: .05rem .35rem;
  }
  .ind-def-value blockquote {
    margin: .4rem 0;
    padding: .4rem .85rem;
    border-left: 3px solid var(--primary);
    background: oklch(0.97 0.006 232);
    border-radius: 0 .35rem .35rem 0;
    font-style: italic;
  }
  .ind-def-value table {
    width: 100%; font-size: .82rem;
    border-collapse: collapse; margin: .4rem 0;
  }
  .ind-def-value table th,
  .ind-def-value table td {
    padding: .4rem .75rem;
    border: 1px solid var(--border);
    text-align: left;
  }
  .ind-def-value table th {
    background: var(--muted);
    font-weight: 700; font-size: .72rem;
    text-transform: uppercase; letter-spacing: .05em;
  }
  .ind-def-value table tr:nth-child(even) td {
    background: oklch(0.975 0.003 260);
  }

  /* ── PDF download button ─────────────────────────────────────────────── */
  .ind-def-dl-btn {
    font-size: .75rem !important; font-weight: 600 !important;
    padding: .28rem .75rem !important; border-radius: .45rem !important;
    flex-shrink: 0;
  }
  .ind-def-dl-btn:hover {
    background: var(--primary) !important;
    color: white !important;
    border-color: var(--primary) !important;
  }
"
