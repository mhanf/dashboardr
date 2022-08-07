#' Define list of ui infobox parameters
#'
#' @param df dasboardr dataframe of an element
#' @param r internal r list
#' @param pattern default pattern to evaluate
#' @importFrom shiny validateCssUnit
#' @return a list of infobox parameters

def_ui_val <- function(df, r = NULL, pattern = "^%r%") {
  val <- list()
  var <- colnames(df)
  # internal external value for all declared variables
  for (k in var) {
    val[[k]] <- int_ext_fct(
      x = df[1, k],
      r = r,
      pattern = pattern
    )
    if (is.null(val[[k]])) {
      val[[k]] <- NA
    }
  }

  # card
  val$card <- as.logical(val$card[1])
  if (is.na(val$card)) {
    val$card <- TRUE
  }
  # scrollable
  val$scrollable <- as.logical(val$scrollable[1])
  if (is.na(val$scrollable)) {
    val$scrollable <- FALSE
  }
  # collapsible
  val$collapsible <- as.logical(val$collapsible[1])
  if (is.na(val$collapsible)) {
    val$collapsible <- FALSE
  }
  # has_border
  val$has_border <- as.logical(val$has_border[1])
  if (is.na(val$has_border)) {
    val$has_border <- FALSE
  }
  # item gap
  shiny::validateCssUnit(val$item_gap)
  # tlp_color
  val$tlp_color <- as.character(val$tlp_color[1])
  if (is.na(val$tlp_color)) {
    val$tlp_color <- "black"
  }
  match.arg(
    arg = val$tlp_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # tooltip position
  val$tlp_position <- as.character(val$tlp_position[1])
  if (is.na(val$tlp_position)) {
    val$tlp_position <- "top"
  }
  match.arg(
    arg = val$tlp_position,
    choices = c("top", "bottom", "left", "right"),
    several.ok = FALSE
  )
  # nav_icon_name
  # val$nav_icon_name <- as.character(val$nav_icon_name[1])
  # if (is.na(val$nav_icon_name)) { val$nav_icon_name <- NULL }
  # # nav_icon_lib
  # val$nav_icon_lib <- as.character(val$nav_icon_lib[1])
  # if (is.na(val$nav_icon_lib)) { val$nav_icon_lib <- "font-awesome" }
  # # height
  # val$height <- as.character(val$height[1])
  # if (is.na(val$height)) {
  #   val$el_height <- "300px"
  # }
  # validateCssUnit(val$height)
  # # type
  # match.arg(
  #   arg = val$type,
  #   choices = c("infobox", "gauge", "module"),
  #   several.ok = FALSE
  # )
  # return
  return(val)
}
