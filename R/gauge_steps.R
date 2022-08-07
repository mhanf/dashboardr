#' Compute step parameter for gauge
#'
#' @param indic_val df associated indicator values
#' @param theme_var the list of bslib variables
#'
#' @return a step parameter for a gauge

gauge_steps <- function(indic_val, theme_var) {
  if (length(indic_val$indic_gauge_step_val) > 1) {
    indic_steps <- list(NULL)
    for (k in 1:(length(indic_val$indic_gauge_step_val) - 1)) {
      indic_steps[[k]] <- list(
        range = c(
          indic_val$indic_gauge_step_val[k],
          indic_val$indic_gauge_step_val[k + 1]
        ),
        color = theme_var[[indic_val$indic_gauge_step_color[k]]]
      )
    }
  } else {
    indic_steps <- NA
  }
  return(indic_steps)
}
