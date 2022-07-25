#' Extract variable name
#'
#' @return a list of variable names organized by thematic

extract_var_name <- function() {
  # essential
  essential <- c("id", "type", "data", "row", "section")
  # row
  row <- c(
    "row_align",
    "row_title",
    "row_title_align",
    "row_title_color"
  )
  # section
  section <- c(
    "type",
    "sect_width", "sect_width_sm", "sect_width_md",
    "sect_width_lg", "sect_width_xl", "sect_title",
    "sect_footer", "sect_title_align", "sect_footer_align",
    "sect_title_color", "sect_title_bgcolor", "sect_footer_bgcolor",
    "sect_tlp_msg", "sect_tlp_color", "sect_tlp_position", "el_height"
  )
  # nav
  nav <- c("nav_title", "nav_icon_name", "nav_icon_lib")
  # element
  element <- c("el_height")
  # infobox
  infobox <- c(
    "infobox_title", "infobox_value", "infobox_delta", "infobox_delta_value",
    "infobox_delta_round", "infobox_delta_relative", "infobox_bgcolor",
    "infobox_icon_name", "infobox_icon_lib", "sect_tlp_msg",
    "sect_tlp_color", "sect_tlp_position"
  )
  # indicator
  indic <- c(
    "indic_value", "indic_value_color", "indic_value_prefix",
    "indic_value_suffix", "indic_delta", "indic_delta_value",
    "indic_delta_relative", "indic_delta_invcolor", "indic_delta_format",
    "indic_delta_pos", "indic_gauge", "indic_gauge_color", "indic_gauge_min",
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
  var_name$essential <- essential
  var_name$row <- row
  var_name$section <- section
  var_name$nav <- nav
  var_name$element <- element
  var_name$infobox <- infobox
  var_name$indic <- indic
  var_name$module <- module
  var_name$color <- color
  # return
  return(var_name)
}
