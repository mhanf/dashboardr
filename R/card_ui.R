#' Function to encapsulate a shiny tag into a card
#'
#' @param title title of the card
#' @param body shiny tag to encapsulate
#'
#' @return A shiny tag encapsulated into a card
#' @export

card_ui <- function(
  title,
  body
) {
  div(
    class = "col",
    tags$div(
      class = "card text-center h-100",
      tags$div(
        class = "card-header primary-text fw-bold",
        title
      ) ,
      tags$div(
        class = "card-body",
        body
      )
    )
  )
}
