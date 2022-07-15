
#' Add tooltip to section title
#'
#' @param df_sect df_sect object
#'
#' @import htmltools
#' @import shiny
#' @return a html icon with tooltip
#' @export

add_tooltip <- function(df_sect) {

  # special case of black and white
  tlp_color_css <- ifelse(df_sect$sect_tlp_color == "black" | df_sect$sect_tlp_color == "white",
    df_sect$sect_tlp_color,
    sprintf("var(--bs-%s)", df_sect$sect_tlp_color)
  )

  icon_color <- "text-default"

  # icon
  el <- shiny::icon(
    class = "icon_color",
    "question-circle" # ,
    # style = sprintf("color: %s;",tlp_color_css)
  )
  # message
  title <- df_sect$sect_tlp_msg
  # placement
  placement <- df_sect$sect_tlp_position
  # tooltip
  tooltip <- tagAppendAttributes(
    el,
    title = HTML(as.character(title)),
    `data-bs-placement` = placement,
    `data-bs-toggle` = "tooltip",
    `data-bs-html` = "true",
    `data-bs-trigger` = "hover focus",
    role = "button",
    tabindex = "0",
    `data-bs-custom-class` = sprintf("tooltip-%s", df_sect$sect_tlp_color)
  )
  # tooltip dependence
  tooltip_dep <- htmltools::htmlDependency(
    name = "tooltip",
    version = "0.0.1",
    package = "dashboardr",
    src = "assets",
    script = "tooltip.js",
    stylesheet = c(file = "tooltip.css")
  )
  # attach dependence
  tooltip <- tagList(tooltip_dep, tooltip)

  return(tooltip)
}
