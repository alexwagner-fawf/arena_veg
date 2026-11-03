test_that("the circle details stay on the open tab when another circle is clicked", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  v <- d$verjk[d$verjk$source_survey == 33L, ]
  selection <- shiny::reactiveValues(pk = NULL, verjk = NULL, nonce = NULL)
  url_fun <- function(survey_id, file_uuid, thumb = FALSE, large = FALSE) paste0("f/", file_uuid)
  # bslib marks the open tab on its <li>.
  active_tab <- function(html) {
    m <- regmatches(html, regexpr("<li class=\"active\">\\s*<a[^>]*data-value=\"[^\"]+\"", html))
    sub(".*data-value=\"([^\"]+)\"$", "\\1", m)
  }
  shiny::testServer(mod_karte_server, args = list(
    filtered = shiny::reactive(d), species = shiny::reactive(character()), selection = selection, url_fun = url_fun
  ), {
    selection$pk <- v$pk_uuid[1]
    selection$verjk <- v$verjk_uuid[1]
    session$flushReact()
    expect_equal(active_tab(output$verjk_detail$html), "Bodendecker erw.")

    session$setInputs(verjk_tab = "Fotos")
    selection$verjk <- v$verjk_uuid[2]
    session$flushReact()
    expect_equal(active_tab(output$verjk_detail$html), "Fotos")
  })
})
