# Arena's default record steps.
step_labels <- c("1" = "Erfassung", "2" = "Bereinigung", "3" = "Analyse")

#' Filter for record step and cycle
#'
#' Choices are taken from the `record_step` and `record_cycle` columns of the
#' data; all are selected initially. Cycles are shown 1-based, as in Arena.
#'
#' @param id Module id.
#' @export
mod_step_cycle_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::checkboxGroupInput(ns("step"), "Schritt", choices = character()),
    shiny::checkboxGroupInput(ns("cycle"), "Zyklus", choices = character())
  )
}

#' @rdname mod_step_cycle_filter_ui
#' @param data Reactive data frame with `record_step` and `record_cycle`.
#' @return A reactive `list(step, cycle)` of the selected values.
#' @export
mod_step_cycle_filter_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      steps <- sort(unique(stats::na.omit(data()$record_step)))
      cycles <- sort(unique(stats::na.omit(data()$record_cycle)))
      shiny::updateCheckboxGroupInput(session, "step", selected = steps, choices = stats::setNames(
        steps, ifelse(is.na(step_labels[steps]), steps, step_labels[steps])
      ))
      shiny::updateCheckboxGroupInput(session, "cycle", selected = cycles, choices = stats::setNames(
        cycles, paste("Zyklus", as.integer(cycles) + 1)
      ))
    })
    shiny::reactive(list(step = input$step, cycle = input$cycle))
  })
}

#' Apply a step/cycle selection
#'
#' @param df Data frame with `record_step` and `record_cycle`.
#' @param sel Selection from [mod_step_cycle_filter_server()].
#' @return The rows of `df` matching the selection.
#' @export
filter_step_cycle <- function(df, sel) {
  df[df$record_step %in% sel$step & df$record_cycle %in% sel$cycle, ]
}
