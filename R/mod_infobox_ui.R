
#' infobox ui part
#'
#' @param id module id
#' @param df_graph A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#' @param default_pattern default pattern to identify r code in dashboardr dataframe
#' @importFrom shiny NS tagList uiOutput div icon
#' @return an infobox ui

mod_infobox_ui <- function(id, df_graph, r = NULL, default_pattern = "^%r%") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(
      outputId = ns("infobox"),
      class = "h-100 w-100 m-0 position-relative"
    ),
    shiny::div(
      class = "card-title position-absolute top-0 end-0 m-2",
      if (!is.na(df_graph$sect_tlp_msg[1])) {
        add_tooltip(df_graph, shiny::icon("question-circle", style = "color:transparent;"))
      }
    )
  )
}
