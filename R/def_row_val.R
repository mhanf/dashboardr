#' Define list of row parameters
#'
#' @param df_row graphical element dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#'
#' @return a list of row parameters

def_row_val <- function(df_row, r = NULL, default_pattern = "^%r%") {
  # bootstrap color
  bs_color <- c(
    "default", "primary", "secondary", "light", "dark", "info",
    "danger", "warning", "success", "black", "white"
  )
  # internal external value
  row_title <- int_ext_fct(
    x = df_row$row_title[1],
    r = r,
    default_pattern = default_pattern
  )
  # title align
  row_title_align <- int_ext_fct(
    x = df_row$row_title_align[1],
    r = r,
    default_pattern = default_pattern
  )
  if (is.na(row_title_align[1])) {
    row_title_align <- "center"
  }
  match.arg(
    arg = row_title_align,
    choices = c("start", "center", "end"),
    several.ok = FALSE
  )
  # title color
  row_title_color <- int_ext_fct(
    x = df_row$row_title_color[1],
    r = r,
    default_pattern = default_pattern
  )
  ## default value
  if (is.na(row_title_color[1])) {
    row_title_color <- "default"
  }
  ## tcheck validity
  match.arg(
    arg = row_title_color,
    choices = bs_color,
    several.ok = FALSE
  )
  ## add class type
  row_title_color <- ifelse(
    row_title_color == "default",
    "",
    sprintf("text-%s", row_title_color)
  )
  # list compilation
  row_val <- list(NULL)
  row_val$row_title <- row_title
  row_val$row_title_align <- row_title_align
  row_val$row_title_color <- row_title_color
  # return
  return(row_val)
}
