#' Define list of element parameters
#'
#' @param df_graph graphical element dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#' @importFrom shiny validateCssUnit
#' @return a list of element parameters

def_el_val <- function(df_graph, r = NULL, default_pattern = "^%r%") {
  # internal external value
  el_height <- int_ext_fct(
    x = df_graph$el_height[1],
    r = r,
    default_pattern = default_pattern
  )
  # validity test
  shiny::validateCssUnit(el_height)
  # default values
  if (is.na(el_height) & df_graph$type[1] == "plot") {
    el_height <- "350px"
  } else if (is.na(el_height) & df_graph$type[1] == "indicator") {
    el_height <- "150px"
  } else if (is.na(el_height) & df_graph$type[1] == "table") {
    el_height <- "auto"
  }
  # list compilation
  el_val <- list(NULL)
  el_val$el_height <- el_height
  # return
  return(el_val)
}
