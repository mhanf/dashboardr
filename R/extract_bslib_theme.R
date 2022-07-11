#' Extract values of selected bslib variables
#'
#' @param session shiny session
#' @importFrom bslib bs_current_theme bs_get_variables
#' @return List of values for selected bslib variables

extract_bslib_theme <- function(session) {
  # theme identification
  theme <- bslib::bs_current_theme(session = session)
  # var to collect
  var_theme_bs <- c(
    "body-bg",
    "body-color",
    "primary",
    "secondary",
    "light",
    "dark",
    "info",
    "success",
    "warning",
    "danger",
    "font-family-base"
  )
  # value of collected var
  var_value_bs <- bslib::bs_get_variables(theme = theme, varnames = var_theme_bs)
  # list compilation
  theme_bs_var <- list(NULL)
  for (i in 1:length(unique(var_theme_bs))) {
    theme_bs_var[[var_theme_bs[i]]] <- as.vector(var_value_bs[i])
  }
  # return
  return(theme_bs_var)
}
