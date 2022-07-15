#' Define list of card parameters
#'
#' @param df_sect section dasboardr dataframe
#' @param r internal r list
#' @param default_pattern default pattern to evaluate
#'
#' @return a list of card parameters

def_sect_val <- function(df_sect, r = NULL, default_pattern = "^%r%") {
  # bootstrap color
  bs_color <- c(
    "default", "primary", "secondary", "light", "dark", "info",
    "danger", "warning", "success", "black", "white"
  )
  # width section
  sect_width <- int_ext_fct(
    x = df_sect$sect_width[1],
    r = r,
    default_pattern = default_pattern
  )
  sect_width_sm <- int_ext_fct(
    x = df_sect$sect_width_sm[1],
    r = r,
    default_pattern = default_pattern
  )
  sect_width_md <- int_ext_fct(
    x = df_sect$sect_width_md[1],
    r = r,
    default_pattern = default_pattern
  )
  sect_width_lg <- int_ext_fct(
    x = df_sect$sect_width_lg[1],
    r = r,
    default_pattern = default_pattern
  )
  sect_width_xl <- int_ext_fct(
    x = df_sect$sect_width_xl[1],
    r = r,
    default_pattern = default_pattern
  )
  # width default value
  sect_width <- ifelse(is.na(sect_width), 12, sect_width)
  match.arg(
    arg = as.character(sect_width),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_width_sm <- ifelse(
    is.na(sect_width_sm),
    as.character(min(as.numeric(sect_width), 12, na.rm = TRUE)),
    sect_width_sm
  )
  match.arg(
    arg = as.character(sect_width_sm),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_width_md <- ifelse(
    is.na(sect_width_md),
    as.character(min(as.numeric(sect_width_sm), 12, na.rm = TRUE)),
    sect_width_md
  )
  match.arg(
    arg = as.character(sect_width_md),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_width_lg <- ifelse(
    is.na(sect_width_lg),
    as.character(min(as.numeric(sect_width_md), 12, na.rm = TRUE)),
    sect_width_lg
  )
  match.arg(
    arg = as.character(sect_width_lg),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  sect_width_xl <- ifelse(
    is.na(sect_width_xl),
    as.character(min(as.numeric(sect_width_lg), 12, na.rm = TRUE)),
    sect_width_xl
  )
  match.arg(
    arg = as.character(sect_width_xl),
    choices = as.character(c(1:12)),
    several.ok = FALSE
  )
  # title
  sect_title <- int_ext_fct(
    x = df_sect$sect_title[1],
    r = r,
    default_pattern = default_pattern
  )
  # footer
  sect_footer <- int_ext_fct(
    x = df_sect$sect_footer[1],
    r = r,
    default_pattern = default_pattern
  )
  # title align
  sect_title_align <- int_ext_fct(
    x = df_sect$sect_title_align[1],
    r = r,
    default_pattern = default_pattern
  )
  if (is.na(sect_title_align[1])) {
    sect_title_align <- "center"
  }
  match.arg(
    arg = sect_title_align,
    choices = c("start", "center", "end"),
    several.ok = FALSE
  )
  # footer align
  sect_footer_align <- int_ext_fct(
    x = df_sect$sect_footer_align[1],
    r = r,
    default_pattern = default_pattern
  )
  if (is.na(sect_footer_align[1])) {
    sect_footer_align <- "center"
  }
  match.arg(
    arg = sect_footer_align,
    choices = c("start", "center", "end"),
    several.ok = FALSE
  )
  # title bgcolor
  ## external compilation
  sect_title_bgcolor <- int_ext_fct(
    x = df_sect$sect_title_bgcolor[1],
    r = r,
    default_pattern = default_pattern
  )
  ## default value
  if (is.na(sect_title_bgcolor[1])) {
    sect_title_bgcolor <- "default"
  }
  ## tcheck validity
  match.arg(
    arg = sect_title_bgcolor,
    choices = bs_color,
    several.ok = FALSE
  )
  ## add class type
  sect_title_bgcolor <- ifelse(
    sect_title_bgcolor == "default",
    "",
    sprintf("bg-%s", sect_title_bgcolor)
  )

  # footer bgcolor
  ## external compilation
  sect_footer_bgcolor <- int_ext_fct(
    x = df_sect$sect_footer_bgcolor[1],
    r = r,
    default_pattern = default_pattern
  )
  ## default value
  if (is.na(sect_footer_bgcolor[1])) {
    sect_footer_bgcolor <- "default"
  }
  ## tcheck validity
  match.arg(
    arg = sect_footer_bgcolor,
    choices = bs_color,
    several.ok = FALSE
  )
  ## add class type
  sect_footer_bgcolor <- ifelse(
    sect_footer_bgcolor == "default",
    "",
    sprintf("bg-%s", sect_footer_bgcolor)
  )

  # tooltip message
  sect_tlp_msg <- int_ext_fct(
    x = df_sect$sect_tlp_msg[1],
    r = r,
    default_pattern = default_pattern
  )
  # tooltip color
  sect_tlp_color <- int_ext_fct(
    x = df_sect$sect_tlp_color[1],
    r = r,
    default_pattern = default_pattern
  )
  if (is.na(sect_tlp_color[1])) {
    sect_tlp_color <- "primary"
  }
  match.arg(
    arg = sect_tlp_color,
    choices = bs_color,
    several.ok = FALSE
  )
  # tooltip position
  sect_tlp_position <- int_ext_fct(
    x = df_sect$sect_tlp_position[1],
    r = r,
    default_pattern = default_pattern
  )
  if (is.na(sect_tlp_position[1])) {
    sect_tlp_position <- "top"
  }
  match.arg(
    arg = sect_tlp_position,
    choices = c("top", "bottom", "left", "right"),
    several.ok = FALSE
  )

  # final values
  sect_val <- list(NULL)
  sect_val$sect_width <- sect_width
  sect_val$sect_width_sm <- sect_width_sm
  sect_val$sect_width_md <- sect_width_md
  sect_val$sect_width_lg <- sect_width_lg
  sect_val$sect_width_xl <- sect_width_xl
  sect_val$sect_title <- sect_title
  sect_val$sect_footer <- sect_footer
  sect_val$sect_title_align <- sect_title_align
  sect_val$sect_footer_align <- sect_footer_align
  sect_val$sect_title_bgcolor <- sect_title_bgcolor
  sect_val$sect_footer_bgcolor <- sect_footer_bgcolor
  sect_val$sect_tlp_msg <- sect_tlp_msg
  sect_val$sect_tlp_color <- sect_tlp_color
  sect_val$sect_tlp_position <- sect_tlp_position
  # return
  return(sect_val)
}
