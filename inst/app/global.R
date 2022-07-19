# library
library(shiny)
library(bslib)
library(dashboardr)
library(readxl)

# dashboardr dataframe
df <- read_excel("www/data_dashboardr.xlsx")

# theme
theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "cerulean",
)

# example module part ui
counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label, class = "btn-danger"),
    p(textOutput(ns("out"), inline = FALSE))
  )
}

# example module part server
counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}
