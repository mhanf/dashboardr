#' Function to encapsulate a shiny tag into a card
#'
#' @param title title of the card
#' @param df_graph A graph dashboarder dataframe
#' @param r r internal list (advanced use)
#' @param body shiny tag to encapsulate
#'
#' @importFrom shiny tags div
#' @return A shiny tag encapsulated into a card
#' @export

card_ui <- function(title,
                    body,
                    df_graph,
                    r = NULL) {
  div(class = "col-4 m-0 p-1",
      tags$div(
        class = "card text-center h-100 m-0",
        tags$div(class = "card-header primary-text fw-bold",
                 title) ,
        tags$div(class = "card-body",
                 body)
      ))
}
