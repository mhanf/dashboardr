
#' Add tooltip to section title
#'
#' @param df_sect df_sect object
#' @param el element for which to add a tooltip
#'
#' @importFrom htmltools htmlDependency
#' @importFrom  shiny icon tagList tagAppendAttributes HTML
#' @return a html icon with tooltip
#' @export

add_tooltip <- function(df_sect, el = shiny::icon("question-circle")) {
  # special case of black and white
  tlp_color_css <- ifelse(df_sect$sect_tlp_color == "black" | df_sect$sect_tlp_color == "white",
    df_sect$sect_tlp_color,
    sprintf("var(--bs-%s)", df_sect$sect_tlp_color)
  )
  # message
  title <- df_sect$sect_tlp_msg
  # placement
  placement <- df_sect$sect_tlp_position
  # tooltip
  tooltip <- shiny::tagAppendAttributes(
    el,
    title = shiny::HTML(as.character(title)),
    `data-bs-placement` = placement,
    `data-bs-toggle` = "tooltip",
    `data-bs-html` = "true",
    `data-bs-trigger` = "hover focus",
    role = "button",
    tabindex = "0",
    `data-bs-custom-class` = sprintf("tooltip-%s", df_sect$sect_tlp_color)
  )
  # return
  return(tooltip)
}
