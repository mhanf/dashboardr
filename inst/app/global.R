library(shiny)
library(bslib)
library(dashboardr)

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly"
)

df <- mtcars
