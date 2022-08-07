
#' Augmented grid-positioned card element
#'
#' @param ... Parameters of grid_card function of the gridlayout package
#' @param title_class class for card-title
#' @param footer tag to include in the card footer
#' @param footer_class class for card-footer
#' @importFrom gridlayout grid_card
#' @importFrom htmltools findDependencies tagQuery tagList div
#' @return Augmented grid-positioned card element
#' @export

grid_card2 <- function(...,
                       title_class = NULL,
                       footer = NULL,
                       footer_class = NULL) {
  # identify dependencies
  dep <- htmltools::findDependencies(gridlayout::grid_card(...))
  # add optional class for card-header
  tag <- htmltools::tagQuery(
    gridlayout::grid_card(...)
  )$
    find(".card-header")$
    addClass(title_class)$
    allTags()
  # add optional card-footer and assiociated class
  if (!is.null(footer)) {
    tag <- htmltools::tagQuery(tag)$
      append(div(
      class = paste("card-footer", footer_class),
      style = "justify-content: space-between;",
      footer
    ))$
      prepend(div(
      class = paste("card-header m-0 p-0"),
      style = "justify-content: space-between; height:0; visibility: hidden;",
      ""
    ))$
      allTags()
  }
  # attach dependencies
  tag <- htmltools::tagList(tag, dep)
  # return
  return(tag)
}
