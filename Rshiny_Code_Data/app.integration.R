library(shiny)
library(shinythemes)
library(dplyr)
library(Rtsne)
library(umap)

UI <- shinyUI(tagList(
  navbarPage("PhenoMultiOmics",
             theme = shinytheme("flatly"),
             source("ui.integration.R",local = TRUE)$value
  )
))

Server <- shinyServer(function(input,output,session){
  source("server.integration.R",local = TRUE)}
)

shinyApp(UI,Server)
