library(shiny)
library(shinythemes)
library(DT)
library(visNetwork)
library(igraph)
library(tidyr)
library(webshot)



# options(shiny.maxRequestSize=60*1024*5,warn = -1)

UI <- shinyUI(tagList(
  navbarPage("PhenoMultiOmics",
             theme = shinytheme("flatly"),
             source("ui.omicsnetwork.R",local = TRUE)$value
  ))
)

Server <- shinyServer(function(input,output,session){
  source("server.omicsnetwork.R",local = TRUE)}
)

shinyApp(UI,Server)



