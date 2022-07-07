ui <- shiny::bootstrapPage(
  theme = theme,
  # mod ui
  mod_dashboardr_ui(id = "dashboard1", df = df, r = NULL)
)
