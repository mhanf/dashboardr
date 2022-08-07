#' infobox server part
#'
#' @param id module id
#' @param df A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#' @param theme_var bslib theme variables
#' @param pattern default pattern to identify r code in dashboardr dataframe
#' @importFrom shiny moduleServer renderUI tagList div tags icon p h3 reactiveValues
#' @return an infobox server

mod_infobox_server <- function(id,
                               df,
                               r = NULL,
                               theme_var,
                               pattern = "^%r%") {
  shiny::moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns

    rbox <- shiny::reactiveValues()
    rbox$bgcolor <- "init"

    output$infobox <- shiny::renderUI({

      # parameter values
      val <- def_infobox_val_server(
        df = df,
        r = r,
        pattern = pattern
      )
      # card color class
      if (rbox$bgcolor != val$infobox_bgcolor) {
        shinyjs::addClass(id = "card", class = sprintf("bg-%s", val$infobox_bgcolor), asis = FALSE)
        shinyjs::removeClass(id = "card", class = sprintf("bg-%s", rbox$bgcolor), asis = FALSE)
        rbox$bgcolor <- val$infobox_bgcolor
      }
      # box render

      # tag computation
      shiny::tagList(
        div(
          class = "card-title position-absolute top-0 start-0 m-2",
          shiny::tags$small(toupper(val$infobox_title))
        ),
        # value
        shiny::div(
          class = "card-title position-absolute bottom-0 start-0 m-2",
          shiny::h3(val$infobox_value,
            class = "m-0 p-0 text-start"
          ),
          if (val$infobox_delta == TRUE) {
            shiny::p(
              class = "m-0 p-0 text-start",
              shiny::icon(
                name = val$infobox_delta_icon,
                class = "m-0 p-0 text-start"
              ),
              val$infobox_delta_value
            )
          } else {
            NULL
          }
        ),
        # icon
        shiny::div(
          class = "card-title position-absolute bottom-0 end-0 m-2",
          shiny::icon(
            name = val$infobox_icon_name,
            lib = val$infobox_icon_lib,
            style = "font-size: 50px;"
          ),
          style = "opacity: 0.2;"
        )
      )
    })
  })
}
