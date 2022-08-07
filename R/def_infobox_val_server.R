#' Define list of infobox parameters
#'
#' @param df graphical element dasboardr dataframe
#' @param r internal r list
#' @param pattern default pattern to evaluate
#' @return a list of infobox parameters

def_infobox_val_server <- function(df, r = NULL, pattern = "^%r%") {
  val <- list()
  infobox_var <- extract_var_name()$infobox

  # internal external value for all declared variables
  for (k in infobox_var) {
    val[[k]] <- int_ext_fct(
      x = df[1, k],
      r = r,
      pattern = pattern
    )
    if (is.null(val[[k]])) {
      val[[k]] <- NA
    }
  }
  # infobox_delta_relative transform
  val$infobox_delta_relative <- as.logical(val$infobox_delta_relative[1])
  if (is.na(val$infobox_delta_relative)) {
    val$infobox_delta_relative <- FALSE
  }
  # infobox_delta_round transform
  val$infobox_delta_round <- as.numeric(val$infobox_delta_round[1])
  if (is.na(val$infobox_delta_round)) {
    val$infobox_delta_round <- 0
  }
  # infobox_title
  val$infobox_title <- val$infobox_title[1]
  if (is.na(val$infobox_title)) {
    val$infobox_title <- ""
  }
  # infobox_bgcolor
  val$infobox_bgcolor <- as.character(val$infobox_bgcolor)[1]
  if (is.na(val$infobox_bgcolor[1])) {
    val$infobox_bgcolor <- "body-color"
  }
  match.arg(
    arg = val$infobox_bgcolor,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # infobox_icon_name
  val$infobox_icon_name <- as.character(val$infobox_icon_name)[1]
  if (is.na(val$infobox_icon_name[1])) {
    val$infobox_icon_name <- NULL
  }
  # infobox_icon_lib
  val$infobox_icon_lib <- as.character(val$infobox_icon_lib)[1]
  if (is.na(val$infobox_icon_lib[1])) {
    val$infobox_icon_lib <- "font-awesome"
  }
  match.arg(
    arg = val$infobox_icon_lib,
    choices = c("font-awesome", "glyphicon"),
    several.ok = FALSE
  )
  # infobox_delta
  val$infobox_delta <- as.logical(val$infobox_delta)[1]
  if (is.na(val$infobox_delta)) {
    val$infobox_delta <- TRUE
  }
  # infobox_value_suffix
  val$infobox_value_suffix <- as.character(val$infobox_value_suffix[1])
  if (is.na(val$infobox_value_suffix)) {
    val$infobox_value_suffix <- ""
  }
  # infobox_delta_suffix
  val$infobox_delta_suffix <- as.character(val$infobox_delta_suffix[1])
  if (is.na(val$infobox_delta_suffix)) {
    val$infobox_delta_suffix <- ""
  }
  # infobox_value & infobox_delta_value
  value_inter <- as.numeric(val$infobox_value)
  ref_inter <- as.numeric(val$infobox_delta_value)
  delta_value_inter <- value_inter - ref_inter
  # infobox delta value
  if (val$infobox_delta_relative == TRUE) {
    delta_value_inter <- round(delta_value_inter / ref_inter * 100, val$infobox_delta_round)
    if (is.na(delta_value_inter)) {
      infobox_delta_icon <- NULL
      delta_value_inter <- "-"
    } else if (delta_value_inter > 0) {
      delta_value_inter <- paste0("+", delta_value_inter)
      infobox_delta_icon <- "arrow-circle-up"
      delta_value_inter <- paste0(delta_value_inter, "%", val$infobox_delta_suffix)
    } else if (delta_value_inter <= 0) {
      infobox_delta_icon <- "arrow-circle-down"
      delta_value_inter <- paste0(delta_value_inter, "%", val$infobox_delta_suffix)
    }
  } else {
    if (is.na(delta_value_inter)) {
      delta_value_inter <- "-"
      infobox_delta_icon <- NULL
    } else if (delta_value_inter > 0) {
      delta_value_inter <- paste0("+", delta_value_inter, val$infobox_delta_suffix)
      infobox_delta_icon <- "arrow-circle-up"
    } else if (delta_value_inter <= 0) {
      delta_value_inter <- paste0(delta_value_inter, val$infobox_delta_suffix)
      infobox_delta_icon <- "arrow-circle-down"
    }
  }
  # value inter
  if (is.na(value_inter)) {
    value_inter <- "-"
  } else {
    value_inter <- paste0(value_inter, val$infobox_value_suffix)
  }

  val$infobox_value <- value_inter
  val$infobox_delta_value <- delta_value_inter
  val$infobox_delta_icon <- infobox_delta_icon
  # return
  return(val)
}
