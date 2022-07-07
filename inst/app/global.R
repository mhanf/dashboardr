library(shiny)
library(bslib)
library(dashboardr)
library(tibble)


theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly"
)

df <- data.frame(id = c("graph1","graph2","graph3","graph4","graph5","graph6"),
               data = rep("mtcars", 6),
               x = c("mpg", "cyl",  "disp", "mpg", "cyl",  "disp"),
               y = c("hp", "drat", "wt", "hp", "drat", "wt"),
               row = c(rep(1,3),rep(2,3))
               )
