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
  # card padding definition
  if ("infobox" %in% sect_val$type) {
    card_p <- 0
  } else {
    card_p <- 1
  }
  # tooltip on card title
  intermed <- sect_val$sect_title
  if (is.na(intermed[1])) {
    intermed <- NULL
  }
  if (!is.na(sect_val$sect_tlp_msg[1]) & sect_val$type[1] != "infobox") {
    sect_title <- div(
      class = "row",
      div(class = sprintf("col-11 text-%s", sect_val$sect_title_align), intermed),
      div(class = "col-1 text-end", add_tooltip(
        df_sect = sect_val,
        el = shiny::icon("question-circle")
      ))
    )
  } else {
    sect_title <- sect_val$sect_title
  }
  # card title
  if (!is.na(sect_title[1])) {
    sect_title <- tags$div(
      class = sprintf(
        "card-header fw-bold %s %s",
        sect_val$sect_title_color,
        sect_val$sect_title_bgcolor
      ),
      sect_title
    )
  } else {
    sect_title <- NULL
  }
  # card footer
  if (!is.na(sect_val$sect_footer[1])) {
    sect_footer <- tags$div(
      class = sprintf(
        "card-footer fs-6 fw-light text-%s %s",
        sect_val$sect_footer_align,
        sect_val$sect_footer_bgcolor
      ),
      sect_val$sect_footer
    )
  } else {
    sect_footer <- NULL
  }

  # card
  card <- tags$div(
    class = "card shadow text-center border-0 h-100 m-0",
    sect_title,
    tags$div(
      class = sprintf("card-body h-100 p-%s m-0", card_p),
      style = sprintf("min-height: %s;", sect_val$el_height), # to improve with height element
      body
    ),
    sect_footer
  )
  # return
  return(card)
}
