ui <- shiny::fluidPage(
  theme = theme,
  # mod ui
  mod_dashboardr_ui(id = "test", df = df)
)
