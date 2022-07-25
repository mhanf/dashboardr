#' infobox server part
#'
#' @param id module id
#' @param df_graph A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#' @param theme_var bslib theme variables
#' @param default_pattern default pattern to identify r code in dashboardr dataframe
#' @importFrom shiny moduleServer renderUI tagList div tags icon p h3
#' @return an infobox server

mod_infobox_server <- function(id, df_graph, r = NULL, theme_var, default_pattern = "^%r%") {
  shiny::moduleServer(id, function(input, output, session) {
    # session ns
    ns <- session$ns
    # infobox info
    output$infobox <- shiny::renderUI({
      infobox_val <- def_infobox_val(
        df_graph = df_graph,
        r = r,
        default_pattern = default_pattern
      )
      # tag computation
      shiny::tagList(
        card <- shiny::div(
          class = sprintf("card h-100 w-100 shadow border-0 m-0 position-relative bg-%s", infobox_val$infobox_bgcolor),
          style = "min-height: 105px;",
          # title
          div(
            class = "card-title position-absolute top-0 start-0 m-2",
            shiny::tags$small(toupper(infobox_val$infobox_title))
          ),
          # tooltip
          shiny::div(
            class = "card-title position-absolute top-0 end-0 m-2",
            if (!is.na(df_graph$sect_tlp_msg[1])) {
              shiny::icon("question-circle")
            }
          ),
          # value
          shiny::div(
            class = "card-title position-absolute bottom-0 start-0 m-2",
            shiny::h3(infobox_val$infobox_value, class = "m-0 p-0 text-start"),
            if (infobox_val$infobox_delta == TRUE) {
              shiny::p(
                shiny::icon(
                  name = infobox_val$infobox_delta_icon,
                  class = "m-0 p-0 text-start"
                ),
                infobox_val$infobox_delta_value,
                class = "m-0 p-0 text-start"
              )
            } else {
              NULL
            }
          ),
          # icon
          shiny::div(
            class = "card-title position-absolute bottom-0 end-0 m-2",
            shiny::icon(
              name = infobox_val$infobox_icon_name,
              lib = infobox_val$infobox_icon_lib,
              style = "font-size: 50px;"
            ),
            style = "opacity: 0.2;"
          )
        )
      )
    })
  })
}
