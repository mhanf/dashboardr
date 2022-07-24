#' Define list of indicator parameters
#'
#' @param df_graph graphical element dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#' @return a list of indicator parameters

def_indic_val <- function(df_graph, r = NULL, default_pattern = "^%r%") {
  indic_val <- list()
  indic_var <- extract_var_name()$indic

  # internal external value for all declared variables
  for (k in indic_var) {
    indic_val[[k]] <- int_ext_fct(
      x = df_graph[1, k],
      r = r,
      default_pattern = default_pattern
    )
  }
  # indic_value_color default value and validity
  if (is.na(indic_val$indic_value_color[1])) {
    indic_val$indic_value_color <- "body-color"
  }
  match.arg(
    arg = indic_val$indic_value_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # indic_delta transform
  indic_val$indic_delta <- as.logical(indic_val$indic_delta[1])
  if (is.na(indic_val$indic_delta[1])) {
    indic_val$indic_delta <- FALSE
  }
  # indic_gauge transform
  indic_val$indic_gauge <- as.logical(indic_val$indic_gauge[1])
  if (is.na(indic_val$indic_gauge[1])) {
    indic_val$indic_gauge <- FALSE
  }
  # indic_type calculation
  indic_val$indic_type <- "number"
  if (indic_val$indic_delta == TRUE) {
    indic_val$indic_type <- paste0(indic_val$indic_type, "+delta")
  }
  if (indic_val$indic_gauge == TRUE) {
    indic_val$indic_type <- paste0(indic_val$indic_type, "+gauge")
  }
  # indic_value_prefix default value
  if (is.na(indic_val$indic_value_prefix[1])) {
    indic_val$indic_value_prefix <- ""
  }
  # indic_value_suffix default value
  if (is.na(indic_val$indic_value_suffix[1])) {
    indic_val$indic_value_suffix <- ""
  }
  # indic_delta_relative transform
  indic_val$indic_delta_relative <- as.logical(indic_val$indic_delta_relative[1])
  if (is.na(indic_val$indic_delta_relative[1])) {
    indic_val$indic_delta_relative <- TRUE
  }
  # indic_delta_invcolor transform
  indic_val$indic_delta_invcolor <- as.logical(indic_val$indic_delta_invcolor[1])
  if (is.na(indic_val$indic_delta_invcolor[1])) {
    indic_val$indic_delta_invcolor <- FALSE
  }
  # calculated indic_incr_color indic_decr_color
  indic_val$indic_incr_color <- "danger"
  indic_val$indic_decr_color <- "success"
  if (indic_val$indic_delta_invcolor == TRUE) {
    indic_val$indic_incr_color <- "success"
    indic_val$indic_decr_color <- "danger"
  }
  # indic_delta_pos default value and validity
  if (is.na(indic_val$indic_delta_pos[1])) {
    indic_val$indic_delta_pos <- "bottom"
  }
  match.arg(
    arg = indic_val$indic_delta_pos,
    choices = c("top", "bottom", "left", "right"),
    several.ok = FALSE
  )
  # indic_delta_format default value
  if (is.na(indic_val$indic_delta_format[1]) & indic_val$indic_delta_relative == TRUE) {
    indic_val$indic_delta_format <- ".0%"
  }
  # indic_gauge_color default value and validity
  if (is.na(indic_val$indic_gauge_color[1])) {
    indic_val$indic_gauge_color <- "body-color"
  }
  match.arg(
    arg = indic_val$indic_gauge_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # indic_gauge_prefix default value
  if (is.na(indic_val$indic_gauge_prefix[1])) {
    indic_val$indic_gauge_prefix <- ""
  }
  # indic_gauge_suffix default value
  if (is.na(indic_val$indic_gauge_suffix[1])) {
    indic_val$indic_gauge_suffix <- ""
  }
  # indic_gauge_step_val normalization
  if (length(indic_val$indic_gauge_step_val) == 1) {
    indic_val$indic_gauge_step_val <- NA
  }
  # indic_gauge_step_color normalization
  if (length(indic_val$indic_gauge_step_val) == 1) {
    indic_val$indic_gauge_step_color <- NA
  }
  if (!is.na(indic_val$indic_gauge_step_val[1]) & length(indic_val$indic_gauge_step_val) != (length(indic_val$indic_gauge_step_color) + 1)) {
    stop("incoherence between indic_gauge_step_val and indic_gauge_step_color")
  }
  indic_val$indic_gauge_step_color[is.na(indic_val$indic_gauge_step_color)] <- "body-bg"
  # indic_tresh_color
  if (is.na(indic_val$indic_tresh_color[1])) {
    indic_val$indic_tresh_color <- "body-color"
  }
  match.arg(
    arg = indic_val$indic_tresh_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # return
  return(indic_val)
}
