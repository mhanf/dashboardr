#' Extract variable name
#'
#' @return a list of variable names organized by thematic

extract_var_name <- function() {
  ui <- c(
    "element", "card", "class", "title", "title_class", "footer",
    "footer_class", "body_class", "scrollable", "collapsible", "has_border",
    "item_gap", "tlp_msg", "tlp_color", "tlp_position",
    "nav_title", "nav_icon_name", "nav_icon_lib", "id", "height", "type"
  )

  # infobox
  infobox <- c(
    "infobox_title", "infobox_value", "infobox_delta", "infobox_delta_value",
    "infobox_delta_round", "infobox_delta_relative", "infobox_bgcolor",
    "infobox_icon_name", "infobox_icon_lib", "infobox_value_suffix",
    "infobox_delta_suffix"
  )
  # indicator
  gauge <- c(
    "indic_value", "indic_value_color", "indic_value_prefix",
    "indic_value_suffix", "indic_delta", "indic_delta_value",
    "indic_delta_relative", "indic_delta_invcolor", "indic_delta_format",
    "indic_gauge", "indic_gauge_color", "indic_gauge_min",
    "indic_gauge_max", "indic_gauge_step_val", "indic_gauge_step_color",
    "indic_gauge_prefix", "indic_gauge_suffix",
    "indic_tresh_value", "indic_tresh_color"
  )
  # module
  module <- c("mod_ui", "mod_server")
  # bootstrap 5 color
  color <- c(
    "body-color", "body-bg", "primary",
    "secondary", "light", "dark", "info",
    "danger", "warning", "success", "black",
    "white"
  )
  # compilation
  var_name <- list(NULL)
  var_name$ui <- ui
  var_name$infobox <- infobox
  var_name$gauge <- gauge
  var_name$module <- module
  var_name$color <- color
  # return
  return(var_name)
}
