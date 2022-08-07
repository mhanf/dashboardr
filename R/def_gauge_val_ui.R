#' Define list of ui gauge parameters
#'
#' @param df dasboardr dataframe of an element
#' @param r internal r list
#' @param pattern default pattern to evaluate
#' @importFrom shiny validateCssUnit
#' @return a list of gauge parameters

def_gauge_val_ui <- function(df, r = NULL, pattern = "^%r%") {
  val <- list()
  var <- c("el_height")

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
  # el_height
  val$el_height <- as.character(val$el_height[1])
  if (is.na(val$el_height)) {
    val$el_height <- "200px"
  }
  shiny::validateCssUnit(val$el_height)

  # return
  return(val)
}
