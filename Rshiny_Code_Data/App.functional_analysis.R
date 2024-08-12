library(shiny)
library(shinythemes)
library(DT)
library(igraph)
library(tidyr)
library(readr)
library(limma)
library(clusterProfiler)
library(datasets)
library(shinydashboard)
library(shinyjs)
library(FactoMineR)
library(factoextra)
library(ropls)
library('mixOmics')
library(org.Hs.eg.db)
library(enrichplot)
library(DOSE)
library(Rtsne)


UI <- shinyUI(tagList(
  navbarPage("PhenoMultiOmics",
             theme = shinytheme("flatly"),
             source("ui.functional_analysis.R",local = TRUE)$value
  ))
)

Server <- shinyServer(function(input,output,session){
  source("server.functional_analysis.R",local = TRUE)}
)

shinyApp(UI,Server)