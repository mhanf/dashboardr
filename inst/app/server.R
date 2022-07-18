
# serveur
server <- function(input, output, session) {
  r <- reactiveValues()
  r$mtcars <- mtcars

  observeEvent(input$click, {
    r$mtcars <- mtcars[mtcars$vs == 1, ]
  })

  # mod server
  mod_dashboardr_server(id = "dashboard1", df = df, r = r)
}
