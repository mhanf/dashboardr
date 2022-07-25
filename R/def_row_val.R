#' Define list of row parameters
#'
#' @param df_row graphical element dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#'
#' @return a list of row parameters

def_row_val <- function(df_row, r = NULL, default_pattern = "^%r%") {
  row_val <- list()
  row_var <- extract_var_name()$row

  # internal external value for all declared variables
  for (k in row_var) {
    row_val[[k]] <- int_ext_fct(
      x = df_row[1, k],
      r = r,
      default_pattern = default_pattern
    )
    if (is.null(row_val[[k]])) {
      row_val[[k]] <- NA
    }
  }
  # row_title_align
  if (is.na(row_val$row_title_align[1])) {
    row_val$row_title_align <- "center"
  }
  match.arg(
    arg = row_val$row_title_align,
    choices = c("start", "center", "end"),
    several.ok = FALSE
  )
  # row_title_color
  if (is.na(row_val$row_title_color[1])) {
    row_val$row_title_color <- "secondary"
  }
  match.arg(
    arg = row_val$row_title_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # row_align
  if (is.na(row_val$row_align[1])) {
    row_val$row_align <- "center"
  }
  match.arg(
    arg = row_val$row_align,
    choices = c("start", "center", "around", "between", "end"),
    several.ok = FALSE
  )
  # return
  return(row_val)
}
