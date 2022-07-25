#' shiny tag encapsulation into a row
#'
#' @param row a row to encapsulate
#' @param df_row A row dashboardr dataframe
#' @param r r internal list (advanced use)
#' @param default_pattern default pattern to evaluate
#'
#' @importFrom shiny tagList div h3
#' @return shiny tag encapsulated into a row

row_ui <- function(row, df_row, r, default_pattern) {
  # row val calculation
  row_val <- def_row_val(
    df_row = df_row,
    r = r,
    default_pattern = default_pattern
  )
  # row encapsulation
  row <- shiny::div(
    class = sprintf("row m-0 p-0 justify-content-%s", row_val$row_align),
    row
  )
  # row title
  if (!is.na(row_val$row_title[1])) {
    row <- shiny::tagList(
      shiny::h3(
        row_val$row_title,
        class = sprintf(
          fmt = "text-%s text-%s m-0 pt-4 px-1 pb-2",
          row_val$row_title_color,
          row_val$row_title_align
        )
      ),
      row
    )
  }
  # return
  return(row)
}
