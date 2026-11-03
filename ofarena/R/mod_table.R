#' Reactable for Arena data
#'
#' Column headers come from `labels` (see [arena_column_labels()]).
#'
#' @param df A data frame.
#' @param labels Named character vector of column labels.
#' @param hidden Columns to hide.
#' @param columns Named list of [reactable::colDef()] overrides.
#' @param ... Passed on to [reactable::reactable()], overriding the defaults
#'   (compact, striped, 15 rows per page).
#' @return A reactable widget.
#' @export
arena_reactable <- function(df, labels = NULL, hidden = character(), columns = list(), ...) {
  df <- sf::st_drop_geometry(df)
  defs <- lapply(names(df), function(col) {
    if (!is.null(columns[[col]])) return(columns[[col]])
    label <- if (!is.null(labels) && !is.na(labels[col])) unname(labels[col]) else col
    reactable::colDef(name = label, show = !col %in% hidden)
  })
  names(defs) <- names(df)
  defaults <- list(compact = TRUE, striped = TRUE, highlight = TRUE, resizable = TRUE, wrap = FALSE,
                   defaultPageSize = 15, language = reactable_de())
  do.call(reactable::reactable, c(list(df, columns = defs), utils::modifyList(defaults, list(...))))
}

#' Photo thumbnail column
#'
#' Shows a thumbnail that opens the original in a new tab.
#'
#' @param df The data frame shown in the table.
#' @param url_fun URL builder from [arena_file_server()].
#' @param name Column header.
#' @param survey_col Column holding the survey id.
#' @param size Thumbnail height in pixels.
#' @return A [reactable::colDef()].
#' @export
photo_col <- function(df, url_fun, name = "Foto", survey_col = "source_survey", size = 56) {
  reactable::colDef(
    name = name,
    minWidth = size + 24,
    sortable = FALSE,
    filterable = FALSE,
    cell = function(value, index) {
      if (is.na(value)) return("")
      survey <- df[[survey_col]][index]
      htmltools::tags$a(
        href = url_fun(survey, value), target = "_blank",
        htmltools::tags$img(
          src = url_fun(survey, value, thumb = TRUE), loading = "lazy",
          style = paste0("height:", size, "px;border-radius:4px;")
        )
      )
    }
  )
}

reactable_de <- function() {
  reactable::reactableLang(
    searchPlaceholder = "Suchen",
    noData = "Keine Daten",
    pageInfo = "{rowStart}\u2013{rowEnd} von {rows}",
    pagePrevious = "\u2039",
    pageNext = "\u203a",
    pagePreviousLabel = "Vorherige Seite",
    pageNextLabel = "N\u00e4chste Seite",
    pageSizeOptions = "{rows} Zeilen"
  )
}

#' Table module with row selection
#'
#' @param id Module id.
#' @export
mod_table_ui <- function(id) {
  reactable::reactableOutput(shiny::NS(id, "table"))
}

#' @rdname mod_table_ui
#' @param data Reactive data frame.
#' @param labels Column labels, as in [arena_reactable()]; a value or a
#'   reactive.
#' @param hidden Columns to hide.
#' @param columns Named list of column definitions, or a function of the data
#'   returning one (for cells that need the data, like [photo_col()]).
#' @param selectable Allow selecting a single row.
#' @param selected Optional reactive returning the row index to select when
#'   the table is drawn (e.g. a selection made elsewhere in the app).
#' @param ... Passed on to [arena_reactable()].
#' @return A list with `selected` (reactive: the selected row of `data()` or
#'   `NULL`).
#' @export
mod_table_server <- function(id, data, labels = NULL, hidden = character(), columns = list(),
                             selectable = FALSE, selected = NULL, ...) {
  args <- list(...)
  shiny::moduleServer(id, function(input, output, session) {
    output$table <- reactable::renderReactable({
      df <- data()
      cols <- if (is.function(columns)) columns(df) else columns
      lab <- if (shiny::is.reactive(labels)) labels() else labels
      extra <- list()
      if (selectable) {
        i <- if (is.null(selected)) NULL else selected()
        extra <- list(selection = "single", onClick = "select",
                      defaultSelected = if (length(i) == 1 && !is.na(i)) i)
      }
      do.call(arena_reactable, c(list(df, labels = lab, hidden = hidden, columns = cols), extra, args))
    })

    # Depends on the table state only: when data() changes, the old selection
    # index must not be applied to the new rows before the table is redrawn.
    selected_row <- shiny::reactive({
      if (!selectable) return(NULL)
      i <- reactable::getReactableState("table", "selected")
      df <- shiny::isolate(data())
      if (is.null(i) || i > nrow(df)) NULL else df[i, ]
    })

    list(selected = selected_row)
  })
}
