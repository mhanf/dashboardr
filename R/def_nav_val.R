#' Define list of nav parameters
#'
#' @param df_graph graphical element dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#'
#' @return a list of nav parameters

def_nav_val <- function(df_graph, r = NULL, default_pattern = "^%r%") {
  # internal external value
  nav_title <- int_ext_fct(
    x = df_graph$nav_title[1],
    r = r,
    default_pattern = default_pattern
  )
  nav_icon_name <- int_ext_fct(
    x = df_graph$nav_icon_name[1],
    r = r,
    default_pattern = default_pattern
  )
  nav_icon_lib <- int_ext_fct(
    x = df_graph$nav_icon_lib[1],
    r = r,
    default_pattern = default_pattern
  )
  # default title if noting is specified
  if (is.na(nav_title[1]) & is.na((nav_icon_name[1]))) {
    nav_title <- df_graph$id[1]
    nav_icon_name <- NULL
    nav_icon_lib <- "font-awesome"
  } else {
    # value
    if (is.na(nav_title[1])) {
      nav_title <- NULL
    }
    if (is.na(nav_icon_name[1])) {
      nav_icon_name <- NULL
    }
    if (is.na(nav_icon_lib[1])) {
      nav_icon_lib <- "font-awesome"
    }
  }
  # list compilation
  nav_val <- list(NULL)
  nav_val$nav_title <- nav_title
  nav_val$nav_icon_name <- nav_icon_name
  nav_val$nav_icon_lib <- nav_icon_lib
  # return
  return(nav_val)
}
