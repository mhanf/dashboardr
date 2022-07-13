#' Function to encapsulate a shiny tag into a card
#'
#' @param sect_val a list of section parameters
#' @param r r internal list (advanced use)
#' @param body shiny tag to encapsulate
#' @importFrom shiny tags div
#' @return A shiny tag encapsulated into a card

card_ui <- function(body,
                    sect_val,
                    r = NULL) {


  # transform color
  sect_title_bgcolor <- ifelse(
    sect_val$sect_title_bgcolor == "default",
    "",
    paste0("bg-", sect_val$sect_title_bgcolor)
  )

  sect_footer_bgcolor <- ifelse(
    sect_val$sect_footer_bgcolor == "default",
    "",
    paste0("bg-", sect_val$sect_footer_bgcolor)
  )

  sect_border_color <- ifelse(
    sect_val$sect_border_color == "default",
    "",
    paste0("border-", sect_val$sect_border_color)
  )
  # title
  if (!is.na(sect_val$sect_title[1])) {
    sect_title <- tags$div(
      class = sprintf(
        "card-header fw-bold text-%s %s %s",
        sect_val$sect_title_align,
        sect_title_bgcolor,
        sect_border_color
      ),
      sect_val$sect_title
    )
  } else {
    sect_title <- NULL
  }
  # footer
  if (!is.na(sect_val$sect_footer[1])) {
    sect_footer <- tags$div(
      class = sprintf(
        "card-footer fs-6 fw-light text-%s %s %s",
        sect_val$sect_footer_align,
        sect_footer_bgcolor,
        sect_border_color
      ),
      sect_val$sect_footer
    )
  } else {
    sect_footer <- NULL
  }
  # card
  card <- tags$div(
    class = sprintf(
      "card text-center h-100 m-0 %s",
      sect_border_color
    ),
    sect_title,
    tags$div(
      class = "card-body p-1 m-0",
      body
    ),
    sect_footer
  )
  # return
  return(card)
}
