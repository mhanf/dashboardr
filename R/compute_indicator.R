#' create an indicator plotly graph
#'
#' @param fig a default plotly indicator
#' @param theme_var the list of bslib variables
#' @param indic_val df associated indicator values
#'
#' @importFrom plotly add_trace
#'
#' @return a new plotly indicator

compute_indicator <- function(fig, indic_val, theme_var) {
  # Indicator computation
  fig <- plotly::add_trace(
    fig,
    type = "indicator",
    mode = indic_val$indic_type,
    value = indic_val$indic_value,
    # title = list(
    #   text = "Hier : 34",
    #   font = list(size = 24,color="red")
    #   ),
    number = list(
      font = list(color = theme_var[[indic_val$indic_value_color]]),
      prefix = indic_val$indic_value_prefix,
      suffix = indic_val$indic_value_suffix
    ),
    # delta variables
    delta = list(
      reference = indic_val$indic_delta_value,
      position = indic_val$indic_delta_pos,
      relative = indic_val$indic_delta_relative,
      valueformat = indic_val$indic_delta_format,
      increasing = list(color = theme_var[[indic_val$indic_incr_color]]),
      decreasing = list(color = theme_var[[indic_val$indic_decr_color]])
    ),
    # gauge variables
    gauge = list(
      axis = list(
        range = list(indic_val$indic_gauge_min, indic_val$indic_gauge_max),
        tickfont = list(color = theme_var$`body-color`),
        tickprefix = indic_val$indic_gauge_prefix,
        ticksuffix = indic_val$indic_gauge_suffix,
        tickcolor = theme_var$`body-color`
      ),
      bar = list(color = theme_var[[indic_val$indic_gauge_color]]),
      bgcolor = theme_var$`body-bg`,
      bordercolor = theme_var$`body-color`,
      steps = compute_gauge_steps(
        indic_val = indic_val,
        theme_var = theme_var
      ),
      # threshold variables
      threshold = list(
        line = list(
          color = theme_var[[indic_val$indic_tresh_color]],
          width = 4
        ),
        thickness = 0.75,
        value = indic_val$indic_tresh_value
      )
    )
  )
  # return
  return(fig)
}
