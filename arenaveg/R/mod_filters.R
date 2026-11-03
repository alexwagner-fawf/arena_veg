#' Sidebar filters
#' @noRd
mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::div(
      class = "d-flex align-items-center gap-2",
      shiny::actionButton(ns("reload"), "Daten neu laden", icon = shiny::icon("rotate"), class = "btn-sm"),
      shiny::textOutput(ns("loaded_at"), inline = TRUE) |> htmltools::tagAppendAttributes(class = "small text-muted")
    ),
    shiny::checkboxGroupInput(ns("surveys"), "Survey", choices = character()),
    shiny::selectizeInput(ns("vf"), "Versuchsfl\u00e4che", choices = NULL, multiple = TRUE,
                          options = list(placeholder = "alle")),
    shiny::selectizeInput(ns("pk_nr"), "Probekreis", choices = NULL, multiple = TRUE,
                          options = list(placeholder = "alle")),
    shiny::selectizeInput(ns("jahr"), "Hauptaufnahmejahr", choices = NULL, multiple = TRUE,
                          options = list(placeholder = "alle")),
    shiny::selectizeInput(ns("gezaeunt"), "Gez\u00e4unt", choices = NULL, multiple = TRUE,
                          options = list(placeholder = "alle")),
    ofarena::mod_step_cycle_filter_ui(ns("step_cycle")),
    shiny::selectizeInput(ns("species"), "Art hervorheben (Bodendecker)", choices = NULL, multiple = TRUE,
                          options = list(placeholder = "Art w\u00e4hlen")),
    shiny::actionLink(ns("reset"), "Filter zur\u00fccksetzen")
  )
}

#' @param data Reactive with the unfiltered data (`veg_assemble()`).
#' @return A list of reactives: `filter` (for `veg_filter()`), `species`
#'   (labels to highlight) and `reload` (button counter).
#' @noRd
mod_filters_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    output$loaded_at <- shiny::renderText(paste("Stand", format(data()$loaded_at, "%H:%M")))

    shiny::observeEvent(data(), {
      d <- data()
      s <- d$surveys
      shiny::updateCheckboxGroupInput(
        session, "surveys",
        choices = stats::setNames(s$id, sprintf("%d \u00b7 %s", s$id, s$name)),
        selected = union(intersect(input$surveys, s$id), if (is.null(input$surveys)) s$id)
      )
      keep <- function(sel, choices) intersect(sel, choices)
      shiny::updateSelectizeInput(session, "vf", choices = sort(unique(d$pk$vf)), selected = keep(input$vf, d$pk$vf))
      shiny::updateSelectizeInput(session, "jahr", choices = sort(unique(d$pk$jahr), decreasing = TRUE),
                                  selected = keep(input$jahr, d$pk$jahr))
      fences <- sort(unique(fence_label(d$verjk$gezauent_label)))
      shiny::updateSelectizeInput(session, "gezaeunt", choices = fences, selected = keep(input$gezaeunt, fences))
      shiny::updateSelectizeInput(session, "species", choices = veg_species_choices(d),
                                  selected = input$species, server = TRUE)
    })

    # Probekreis choices follow the selected Versuchsflächen.
    shiny::observeEvent(list(data(), input$vf), ignoreNULL = FALSE, {
      pk <- data()$pk
      if (length(input$vf) > 0) pk <- pk[pk$vf %in% input$vf, ]
      choices <- lapply(split(pk$pk_nr, pk$vf), function(x) {
        x <- unique(x)
        as.list(x[order(natural_key(x))])
      })
      shiny::updateSelectizeInput(session, "pk_nr", choices = choices,
                                  selected = intersect(input$pk_nr, pk$pk_nr))
    })

    step_cycle <- ofarena::mod_step_cycle_filter_server("step_cycle", shiny::reactive(data()$pk))

    shiny::observeEvent(input$reset, {
      shiny::updateCheckboxGroupInput(session, "surveys", selected = data()$surveys$id)
      for (id in c("vf", "pk_nr", "jahr", "gezaeunt", "species")) {
        shiny::updateSelectizeInput(session, id, selected = character())
      }
    })

    filter <- shiny::reactive({
      sc <- step_cycle()
      list(
        surveys = as.integer(input$surveys), vf = input$vf, pk_nr = input$pk_nr, jahr = input$jahr,
        gezaeunt = input$gezaeunt, step = sc$step, cycle = sc$cycle
      )
    }) |> shiny::debounce(300)

    list(filter = filter, species = shiny::reactive(input$species), reload = shiny::reactive(input$reload))
  })
}
