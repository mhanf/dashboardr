#' Define list of infobox parameters
#'
#' @param df_graph graphical element dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#' @return a list of infobox parameters

def_infobox_val <- function(df_graph, r = NULL, default_pattern = "^%r%") {
  infobox_val <- list()
  infobox_var <- extract_var_name()$infobox

  # internal external value for all declared variables
  for (k in infobox_var) {
    infobox_val[[k]] <- int_ext_fct(
      x = df_graph[1, k],
      r = r,
      default_pattern = default_pattern
    )
    if (is.null(infobox_val[[k]])) {
      infobox_val[[k]] <- NA
    }
  }

  # infobox_delta_relative transform
  infobox_val$infobox_delta_relative <- as.logical(infobox_val$infobox_delta_relative[1])
  if (is.na(infobox_val$infobox_delta_relative)) {
    infobox_val$infobox_delta_relative <- FALSE
  }
  # infobox_delta_round transform
  infobox_val$infobox_delta_round <- as.numeric(infobox_val$infobox_delta_round[1])
  if (is.na(infobox_val$infobox_delta_round)) {
    infobox_val$infobox_delta_round <- 0
  }
  # infobox_value & infobox_delta_value
  value_inter <- as.numeric(infobox_val$infobox_value)
  ref_inter <- as.numeric(infobox_val$infobox_delta_value)
  delta_value_inter <- value_inter - ref_inter
  infobox_delta_icon <- "arrow-circle-down"
  if (infobox_val$infobox_delta_relative == TRUE) {
    delta_value_inter <- round(delta_value_inter / ref_inter * 100, infobox_val$infobox_delta_round)
    if (is.na(delta_value_inter)) {
      delta_value_inter <- "-"
    } else {
      if (delta_value_inter > 0) {
        delta_value_inter <- paste0("+", delta_value_inter)
        infobox_delta_icon <- "arrow-circle-up"
      }
      delta_value_inter <- paste(delta_value_inter, "%")
    }
  } else {
    if (is.na(delta_value_inter)) {
      delta_value_inter <- "-"
    } else if (delta_value_inter >= 0) {
      delta_value_inter <- paste0("+", delta_value_inter)
      infobox_delta_icon <- "arrow-circle-up"
    }
  }
  # value_inter <- as.character(value_inter)
  if (is.na(value_inter)) {
    value_inter <- "-"
  }

  infobox_val$infobox_value <- value_inter
  infobox_val$infobox_delta_value <- delta_value_inter
  infobox_val$infobox_delta_icon <- infobox_delta_icon
  # return
  return(infobox_val)
}
