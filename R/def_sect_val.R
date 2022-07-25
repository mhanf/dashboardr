#' Define list of section parameters
#'
#' @param df_sect section dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#'
#' @return a list of section parameters

def_sect_val <- function(df_sect, r = NULL, default_pattern = "^%r%") {
  sect_val <- list()
  sect_var <- extract_var_name()$section

  # internal external value for all declared variables
  for (k in sect_var) {
    sect_val[[k]] <- int_ext_fct(
      x = df_sect[1, k],
      r = r,
      default_pattern = default_pattern
    )
    if (is.null(sect_val[[k]])) {
      sect_val[[k]] <- NA
    }
  }
  # width value
  sect_val$sect_width <- ifelse(
    is.na(sect_val$sect_width),
    12,
    sect_val$sect_width
  )
  match.arg(
    arg = as.character(sect_val$sect_width),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_val$sect_width_sm <- ifelse(
    is.na(sect_val$sect_width_sm),
    as.character(min(as.numeric(sect_val$sect_width), 12, na.rm = TRUE)),
    sect_val$sect_width_sm
  )
  match.arg(
    arg = as.character(sect_val$sect_width_sm),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_val$sect_width_md <- ifelse(
    is.na(sect_val$sect_width_md),
    as.character(min(as.numeric(sect_val$sect_width_sm), 12, na.rm = TRUE)),
    sect_val$sect_width_md
  )
  match.arg(
    arg = as.character(sect_val$sect_width_md),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_val$sect_width_lg <- ifelse(
    is.na(sect_val$sect_width_lg),
    as.character(min(as.numeric(sect_val$sect_width_md), 12, na.rm = TRUE)),
    sect_val$sect_width_lg
  )
  match.arg(
    arg = as.character(sect_val$sect_width_lg),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_val$sect_width_xl <- ifelse(
    is.na(sect_val$sect_width_xl),
    as.character(min(as.numeric(sect_val$sect_width_lg), 12, na.rm = TRUE)),
    sect_val$sect_width_xl
  )
  match.arg(
    arg = as.character(sect_val$sect_width_xl),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  # title align
  if (is.na(sect_val$sect_title_align[1])) {
    sect_val$sect_title_align <- "center"
  }
  match.arg(
    arg = sect_val$sect_title_align,
    choices = c("start", "center", "end"),
    several.ok = FALSE
  )
  # footer align
  if (is.na(sect_val$sect_footer_align[1])) {
    sect_val$sect_footer_align <- "center"
  }
  match.arg(
    arg = sect_val$sect_footer_align,
    choices = c("start", "center", "end"),
    several.ok = FALSE
  )
  # title color
  if (is.na(sect_val$sect_title_color[1])) {
    sect_val$sect_title_color <- "default"
  }
  match.arg(
    arg = sect_val$sect_title_color,
    choices = c(extract_var_name()$color, "default"),
    several.ok = FALSE
  )
  sect_val$sect_title_color <- ifelse(
    sect_val$sect_title_color == "default",
    "",
    sprintf("text-%s", sect_val$sect_title_color)
  )
  # title bgcolor
  if (is.na(sect_val$sect_title_bgcolor[1])) {
    sect_val$sect_title_bgcolor <- "default"
  }
  match.arg(
    arg = sect_val$sect_title_bgcolor,
    choices = c(extract_var_name()$color, "default"),
    several.ok = FALSE
  )
  sect_val$sect_title_bgcolor <- ifelse(
    sect_val$sect_title_bgcolor == "default",
    "",
    sprintf("bg-%s", sect_val$sect_title_bgcolor)
  )
  # footer bgcolor
  if (is.na(sect_val$sect_footer_bgcolor[1])) {
    sect_val$sect_footer_bgcolor <- "default"
  }
  match.arg(
    arg = sect_val$sect_footer_bgcolor,
    choices = c(extract_var_name()$color, "default"),
    several.ok = FALSE
  )
  sect_val$sect_footer_bgcolor <- ifelse(
    sect_val$sect_footer_bgcolor == "default",
    "",
    sprintf("bg-%s", sect_val$sect_footer_bgcolor)
  )
  # tooltip color
  if (is.na(sect_val$sect_tlp_color[1])) {
    sect_val$sect_tlp_color <- "black"
  }
  match.arg(
    arg = sect_val$sect_tlp_color,
    choices = extract_var_name()$color,
    several.ok = FALSE
  )
  # tooltip position
  if (is.na(sect_val$sect_tlp_position[1])) {
    sect_val$sect_tlp_position <- "top"
  }
  match.arg(
    arg = sect_val$sect_tlp_position,
    choices = c("top", "bottom", "left", "right"),
    several.ok = FALSE
  )
  # el_height
  if (is.na(sect_val$el_height) & "plot" %in% df_sect$type) {
    sect_val$el_height <- "350px"
  } else if (is.na(sect_val$el_height) & "table" %in% df_sect$type) {
    sect_val$el_height <- "350px"
  } else if (is.na(sect_val$el_height) & "indicator" %in% df_sect$type) {
    sect_val$el_height <- "150px"
  } else if (is.na(sect_val$el_height)) {
    el_height <- "100px"
  }
  shiny::validateCssUnit(sect_val$el_height)

  return(sect_val)
}
