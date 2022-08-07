
#' infobox ui part
#'
#' @param id module id
#' @param df A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#' @param pattern default pattern to identify r code in dashboardr dataframe
#' @importFrom shiny NS tagList uiOutput div icon
#' @return an infobox ui

mod_infobox_ui <- function(id,
                           df,
                           r = NULL,
                           pattern = "^%r%") {
  # ns
  ns <- shiny::NS(id)
  # parameter value
  val <- def_infobox_val_ui(
    df = df,
    r = r,
    pattern = pattern
  )
  # tooltip
  tooltip <- NULL
  if (!is.na(val$sect_tlp_msg)) {
    tooltip <- addTooltip(shiny::icon("question-circle"),
      tlp_color = val$sect_tlp_color,
      tlp_msg = val$sect_tlp_msg,
      tlp_position = val$sect_tlp_position
    )
  }
  # tag
  infobox <- shiny::div(
    id = ns("card"),
    class = "card shadow border-0 m-0 position-relative",
    style = paste0("width : 100%; height: ", val$el_height, "; min-height: 100px;"),
    # tooltip
    shiny::div(
      class = "card-title position-absolute top-0 end-0 m-2",
      tooltip
    ),
    shiny::uiOutput(ns("infobox"))
  )
}
