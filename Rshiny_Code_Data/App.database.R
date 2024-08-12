library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(DT)

options(shiny.maxRequestSize=60*1024*5,warn = -1)


UI <- shinyUI(tagList(
  navbarPage("PhenoMultiOmics",
             theme = shinytheme("flatly"),
             source("ui.database.R",local = TRUE)$value
  ))
)

Server <- shinyServer(function(input,output,session){
  source("server.database.R",local = TRUE)}
)

shinyApp(UI,Server)
