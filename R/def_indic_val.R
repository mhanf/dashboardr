#' Define list of indicator parameters
#'
#' @param df_graph graphical element dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#' @return a list of indicator parameters

def_indic_val <- function(df_graph, r = NULL, default_pattern = "^%r%") {
  # indic_value
  ## internal external value
  indic_value <- int_ext_fct(
    x = df_graph$indic_value[1],
    r = r,
    default_pattern = default_pattern
  )
  # indic_value_color
  bs_color <- c(
    "body-color", "primary", "secondary", "light", "dark", "info",
    "danger", "warning", "success", "black", "white"
  )
  ## internal external value
  indic_value_color <- int_ext_fct(
    x = df_graph$indic_value_color[1],
    r = r,
    default_pattern = default_pattern
  )
  ## default value
  if (is.na(indic_value_color[1])) {
    indic_value_color <- "body-color"
  }
  ## tcheck validity
  match.arg(
    arg = indic_value_color,
    choices = bs_color,
    several.ok = FALSE
  )

  # list compilation
  indic_val <- list(NULL)
  indic_val$indic_value <- indic_value
  indic_val$indic_value_color <- indic_value_color
  # return
  return(indic_val)
}
