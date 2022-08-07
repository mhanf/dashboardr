#' Add a tooltip to a \code{shiny.tag}
#' @description Add a tooltip to a \code{shiny.tag}.
#' @param el A \code{shiny.tag} object
#' @param tlp_color The tooltip color, \code{"primary"}, \code{"secondary"}, \code{"light"}, \code{"dark"}, \code{"info"}, \code{"warning"}, \code{"danger"}, \code{"success"}, \code{"white"}, or \code{"black"}.
#' @param tlp_msg The character string to be shown as message in the tooltip.
#' @param tlp_position The tooltip position, \code{"top"}, \code{"right"}, \code{"bottom"}, or \code{"left"}.
#' @importFrom htmltools htmlDependency
#' @importFrom shiny div HTML
#' @return A \code{shiny.tag} object with a tooltip.

addTooltip <- function(el,
                       tlp_color = "primary",
                       tlp_msg,
                       tlp_position = "top") {
  # tooltip
  tooltip <- htmltools::tagAppendAttributes(
    el,
    title = shiny::HTML(as.character(tlp_msg)),
    `data-bs-placement` = tlp_position,
    `data-bs-toggle` = "tooltip",
    `data-bs-html` = "true",
    `data-bs-trigger` = "hover focus",
    role = "button",
    tabindex = "0",
    `data-bs-custom-class` = sprintf("tooltip-%s", tlp_color)
  )

  # tooltip dependence
  tooltip_dep <- htmltools::htmlDependency(
    name = "tooltip",
    version = "0.0.1",
    package = "dashboardr",
    src = "assets",
    script = "tooltip.js",
    stylesheet = "tooltip.css"
  )
  # attach dependence
  tooltip <- shiny::tagList(tooltip_dep, tooltip)
  # return
  return(tooltip)
}
