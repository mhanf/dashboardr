#' Define list of ui infobox parameters
#'
#' @param df dasboardr dataframe of an element
#' @param r internal r list
#' @param pattern default pattern to evaluate
#' @importFrom shiny validateCssUnit
#' @return a list of infobox parameters

def_infobox_val_ui <- function(df, r = NULL, pattern = "^%r%") {
  val <- list()
  var <- c(
    "el_height",
    "sect_tlp_color",
    "sect_tlp_position",
    "sect_tlp_msg"
  )

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

  # tooltip color
  val$sect_tlp_color <- as.character(val$sect_tlp_color[1])
  if (is.na(val$sect_tlp_color)) {
    val$sect_tlp_color <- "black"
  }
  match.arg(
    arg = val$sect_tlp_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # tooltip position
  val$sect_tlp_position <- as.character(val$sect_tlp_position[1])
  if (is.na(val$sect_tlp_position)) {
    val$sect_tlp_position <- "top"
  }
  match.arg(
    arg = val$sect_tlp_position,
    choices = c("top", "bottom", "left", "right"),
    several.ok = FALSE
  )
  # el_height
  val$el_height <- as.character(val$el_height[1])
  if (is.na(val$el_height)) {
    val$el_height <- "100px"
  }
  shiny::validateCssUnit(val$el_height)

  # return
  return(val)
}
