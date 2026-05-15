# ==============================================================================
# Indicator Definitions panel
# Reads data/indicator_definitions.xlsx at startup.
# Each worksheet becomes a tab; each row becomes an expandable indicator card.
# All text goes through htmlEscape + HTML() so the C-locale server never has
# to transcode non-ASCII characters from the workbook.
# ==============================================================================

.DEF_PATH <- "data/indicator_definitions.xlsx"

# Column names as they appear after newline-normalisation.
.DEF_FIELDS <- list(
  list(key = "Definition",      label = "Definition"),
  list(key = "Numerator",       label = "Numerator"),
  list(key = "Denominator",     label = "Denominator"),
  list(key = "Unit of Measure", label = "Unit of Measure"),
  list(key = "Data Source",     label = "Data Source"),
  list(key = "Frequency",       label = "Frequency"),
  list(key = "Disaggregation",  label = "Disaggregation"),
  list(key = "SDG / WHO Ref",   label = "SDG / WHO Ref"),
  list(key = "Notes / Caveats", label = "Notes / Caveats")
)

# ---- Data loading (once at startup) -----------------------------------------

.def_load <- function() {
  if (!requireNamespace("readxl", quietly = TRUE)) return(NULL)
  if (!file.exists(.DEF_PATH)) return(NULL)
  sheets <- readxl::excel_sheets(.DEF_PATH)
  data   <- lapply(sheets, function(s) {
    df        <- readxl::read_excel(.DEF_PATH, sheet = s, col_types = "text")
    names(df) <- trimws(gsub("[\r\n]+", " ", names(df)))
    df
  })
  names(data) <- sheets
  data
}

.def_data <- .def_load()

# ---- Helpers -----------------------------------------------------------------

# Safe HTML: escape then mark as pre-rendered so Shiny skips iconv.
.def_html <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(NULL)
  htmltools::HTML(htmltools::htmlEscape(as.character(x)))
}

.def_field_row <- function(key, label, row) {
  val <- row[[key]]
  txt <- .def_html(val)
  if (is.null(txt)) return(NULL)
  div(class = "def-field",
    div(class = "def-field-label", label),
    div(class = "def-field-value", txt)
  )
}

.def_card <- function(row, card_id) {
  code <- .def_html(row[["Indicator Code"]])
  name <- .def_html(row[["Indicator Name"]])
  if (is.null(name)) return(NULL)

  field_tags <- lapply(.DEF_FIELDS, function(f)
    .def_field_row(f$key, f$label, row)
  )
  field_tags <- Filter(Negate(is.null), field_tags)

  div(class = "def-card",
    div(class = "def-card-header", `data-card` = card_id,
      if (!is.null(code))
        tags$span(class = "def-code-badge", code),
      div(class = "def-card-name", name),
      tags$i(class = "bi bi-chevron-down def-card-chevron")
    ),
    div(id = paste0("def-body-", card_id), class = "def-card-body",
      do.call(tagList, field_tags)
    )
  )
}

.def_sheet_pane <- function(sheet_name, df, idx) {
  safe_id <- gsub("[^a-zA-Z0-9]", "_", sheet_name)
  cards   <- lapply(seq_len(nrow(df)), function(i)
    .def_card(df[i, ], paste0(safe_id, "_", i))
  )
  cards <- Filter(Negate(is.null), cards)

  div(
    id    = paste0("def-sheet-", safe_id),
    class = paste("def-tab-pane", if (idx == 1) "active" else ""),
    div(class = "def-search-wrap",
      div(class = "input-group input-group-sm",
        span(class = "input-group-text",
          tags$i(class = "bi bi-search")
        ),
        tags$input(
          type         = "text",
          class        = "form-control def-search",
          placeholder  = paste("Search", sheet_name, "indicators..."),
          `data-sheet` = safe_id
        )
      )
    ),
    div(class = "def-cards", `data-sheet` = safe_id,
      do.call(tagList, cards)
    )
  )
}

# ---- Public UI ---------------------------------------------------------------

def_panel_ui <- function() {
  if (is.null(.def_data)) {
    return(div(class = "def-panel",
      div(class = "def-empty",
        tags$i(class = "bi bi-file-earmark-excel me-2"),
        "Add ", tags$code("data/indicator_definitions.xlsx"), " to populate this panel."
      )
    ))
  }

  sheet_names <- names(.def_data)

  tab_buttons <- lapply(seq_along(sheet_names), function(i) {
    safe_id <- gsub("[^a-zA-Z0-9]", "_", sheet_names[[i]])
    tags$button(
      class          = paste("def-tab-btn", if (i == 1) "active" else ""),
      type           = "button",
      `data-target`  = paste0("def-sheet-", safe_id),
      sheet_names[[i]]
    )
  })

  tab_panes <- lapply(seq_along(sheet_names), function(i)
    .def_sheet_pane(sheet_names[[i]], .def_data[[i]], i)
  )

  tagList(
    div(class = "def-panel",
      div(class = "def-header",
        div(class = "def-header-inner",
          tags$i(class = "bi bi-book me-2"),
          "Indicator Definitions"
        ),
        tags$p(class = "def-header-sub",
          "Definitions, calculation logic, and data sources for all indicators.",
          " Click any card to expand."
        )
      ),
      div(class = "def-tab-bar",
        do.call(tagList, tab_buttons)
      ),
      div(class = "def-content",
        do.call(tagList, tab_panes)
      )
    )
  )
}
