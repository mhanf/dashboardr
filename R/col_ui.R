#' shiny tag encapsulation into a column
#'
#' @param section shiny tag to encapsulate
#' @param sect_val a list of section parameters
#' @param r r internal list (advanced use)
#'
#' @return shiny tag encapsulated into a column

col_ui <- function(section, sect_val, r) {
  # col class definition
  col_class <- sprintf(
    fmt = "m-0 p-1 col-%s col-sm-%s col-md-%s col-lg-%s col-xl-%s",
    sect_val$sect_width,
    sect_val$sect_width_sm,
    sect_val$sect_width_md,
    sect_val$sect_width_lg,
    sect_val$sect_width_xl
  )
  # col encapsulation
  section <- div(class = col_class, section)
  # return
  return(section)
}
