#' gridcard with tooltips and processing of default values
#'
#' @param area gridcard area
#' @param val ui default value
#' @param r r internal list (advanced uses)
#' @param pattern default pattern to identify r code
#' @importFrom shiny div icon
#' @return a gridcard with tooltips and processing of default values
#' @export

dash_card <- function(area, val, r, pattern) {
  # tooltip
  tooltip <- NULL
  if (!is.na(val$tlp_msg)) {
    tooltip <- div(
      class = "col-1 text-end",
      addTooltip(
        shiny::icon("circle-question"),
        tlp_color = val$tlp_color,
        tlp_msg = val$tlp_msg,
        tlp_position = val$tlp_position
      )
    )
  }
  # class
  if (is.na(val$class)) {
    val$class <- NULL
  }
  # title
  title <- NULL
  if (!is.na(val$title)) {
    title <- shiny::div(
      class = "d-flex d-row w-100",
      shiny::div(class = "col-11 text-left", val$title),
      tooltip
    )
  }
  # footer
  if (is.na(val$footer)) {
    val$footer <- NULL
    val$footer_class <- NULL
  }
  # has_border
  if (is.na(val$has_border)) {
    val$has_border <- NULL
  }
  # card
  card <- grid_card2(
    area,
    title = title,
    title_class = val$title_class,
    footer = val$footer,
    footer_class = val$footer_class,
    has_border = val$has_border,
    class = val$class,
    scrollable = val$scrollable,
    collapsible = val$collapsible,
    item_gap = val$item_gap,
    # body
    paste("card", area)
  )
  # return
  return(card)
}
