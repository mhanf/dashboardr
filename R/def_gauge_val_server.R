#' Define list of infobox parameters
#'
#' @param df graphical element dasboardr dataframe
#' @param r internal r list
#' @param pattern default pattern to evaluate
#' @return a list of infobox parameters

def_gauge_val_server <- function(df, r = NULL, pattern = "^%r%") {
  val <- list()
  var <- extract_var_name()$gauge

  # internal external value for all declared variables
  for (k in var) {
    val[[k]] <- int_ext_fct(
      x = df[1, k],
      r = r,
      pattern = pattern
    )
    if (is.null(val[[k]])) {
      val[[k]] <- NA
    }
  }

  # indic_color
  val$indic_value_color <- as.character(val$indic_value_color[1])
  if (is.na(val$indic_value_color)) {
    val$indic_value_color <- "body-color"
  }
  match.arg(
    arg = val$indic_value_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # indic_delta transform
  val$indic_delta <- as.logical(val$indic_delta[1])
  if (is.na(val$indic_delta[1])) {
    val$indic_delta <- FALSE
  }
  # indic_gauge transform
  val$indic_gauge <- as.logical(val$indic_gauge[1])
  if (is.na(val$indic_gauge[1])) {
    val$indic_gauge <- FALSE
  }
  # indic_type calculation
  val$indic_type <- "number"
  if (val$indic_delta == TRUE) {
    val$indic_type <- paste0(val$indic_type, "+delta")
  }
  if (val$indic_gauge == TRUE) {
    val$indic_type <- paste0(val$indic_type, "+gauge")
  }
  # indic_value_prefix default value
  if (is.na(val$indic_value_prefix[1])) {
    val$indic_value_prefix <- ""
  }
  # indic_value_suffix default value
  if (is.na(val$indic_value_suffix[1])) {
    val$indic_value_suffix <- ""
  }
  # indic_delta_relative transform
  val$indic_delta_relative <- as.logical(val$indic_delta_relative[1])
  if (is.na(val$indic_delta_relative[1])) {
    val$indic_delta_relative <- TRUE
  }
  # indic_delta_invcolor transform
  val$indic_delta_invcolor <- as.logical(val$indic_delta_invcolor[1])
  if (is.na(val$indic_delta_invcolor[1])) {
    val$indic_delta_invcolor <- FALSE
  }
  # calculated indic_incr_color indic_decr_color
  val$indic_incr_color <- "danger"
  val$indic_decr_color <- "success"
  if (val$indic_delta_invcolor == TRUE) {
    val$indic_incr_color <- "success"
    val$indic_decr_color <- "danger"
  }
  # indic_delta_format default value
  if (is.na(val$indic_delta_format[1]) & val$indic_delta_relative == TRUE) {
    val$indic_delta_format <- ".0%"
  }
  # indic_gauge_color default value and validity
  if (is.na(val$indic_gauge_color[1])) {
    val$indic_gauge_color <- "body-color"
  }
  match.arg(
    arg = val$indic_gauge_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # indic_gauge_prefix default value
  if (is.na(val$indic_gauge_prefix[1])) {
    val$indic_gauge_prefix <- ""
  }
  # indic_gauge_suffix default value
  if (is.na(val$indic_gauge_suffix[1])) {
    val$indic_gauge_suffix <- ""
  }
  # indic_gauge_step_val normalization
  if (length(val$indic_gauge_step_val) == 1) {
    val$indic_gauge_step_val <- NA
  }
  # indic_gauge_step_color normalization
  if (length(val$indic_gauge_step_val) == 1) {
    val$indic_gauge_step_color <- NA
  }
  if (!is.na(val$indic_gauge_step_val[1]) & length(val$indic_gauge_step_val) != (length(val$indic_gauge_step_color) + 1)) {
    stop("incoherence between indic_gauge_step_val and indic_gauge_step_color")
  }
  val$indic_gauge_step_color[is.na(val$indic_gauge_step_color)] <- "body-bg"
  # indic_tresh_color
  if (is.na(val$indic_tresh_color[1])) {
    val$indic_tresh_color <- "body-color"
  }
  match.arg(
    arg = val$indic_tresh_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # return
  return(val)
}
