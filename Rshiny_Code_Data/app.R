##https://phenomultiomics.shinyapps.io/cancer/
######Rshiny.App
library(shinydashboard)
library(DT)
library(shiny)
library(datasets)
library(igraph)
library(visNetwork)
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(htmltools)

library(ggpubr)
library(dashboardthemes)
library(shinythemes)
library(shinyjs)

library(limma)
library(pathview)
library(clusterProfiler)
library("org.Hs.eg.db")
library(enrichplot)
library(DOSE)

library(FactoMineR)
library(factoextra)
library(ropls)
library('mixOmics')

library(webshot)
library(shinyWidgets) 
library(colourpicker)
library(htmlwidgets)

library(zip)
library(Rtsne)
library(umap)

library(future)
library(promises)

custom_css <- HTML("
<link href='https://fonts.googleapis.com/css?family=Open+Sans:400,700' rel='stylesheet'>
<style>
body {
  font-family: 'Open Sans', sans-serif;
}

fancyHeader {
  font-weight: 700; 
}

.full-width-image img {
  width: calc(100% - 72px); 
  height: auto; 
  margin: 36px; 
}

.full-width-image {
  padding-left: 36px; 
  padding-right: 36px; 
}

 #dynamicBox {
  overflow-x: auto; 
}

.dataTables_wrapper {
  width: auto !important; 
  overflow-x: scroll;
}

.truncate {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 200px; 
}

.light-gray-text {
   color: gray;
}

 .justify-text {
        text-align: justify;
      }
.stats-container {
        padding: 20px;
        margin-top: 20px;
        background-color: #e9ecef;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      }
      .stats-header {
        font-size: 24px;
        font-weight: bold;
        color: #495057;
      }
      .stats-item {
        font-size: 16px;
        color: #495057;
        margin-bottom: 5px;
      }

</style>
")

UI <- shinyUI(
  tagList(
    useShinyjs(),
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        shinyDashboardThemes(theme = "poor_mans_flatly"),
        navbarPage(
          title = div(
            id = "navbar-title",
            "PhenoMultiOmics"),
          tags$head(
            tags$style(HTML(custom_css)),
            # 引用 Font Awesome CDN
            tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),#"https://use.fontawesome.com/releases/v5.8.2/css/all.css"
            tags$script(HTML("
              $(document).ready(function(){
                $('#navbar-title').click(function(){
                  $('a[data-value=\"Home\"]').click();
                });
              });
            ")),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js")
          ),
                   theme = shinytheme("flatly"),
                   tabPanel("Home", source("ui.home.R", local = TRUE)$value),
                   tabPanel("Database", source("ui.database.R", local = TRUE)$value),
                   tabPanel("Multi-Omics Network", source("ui.omicsnetwork.R", local = TRUE)$value),
                   tabPanel("Multi-Omics Data Integration", source("ui.integration.R", local = TRUE)$value),
                   tabPanel("Biomarker Discovery", source("ui.functional_analysis.R", local = TRUE)$value),
                   tabPanel("Download", source("ui.Download.R", local = TRUE)$value),
                   tabPanel("Documentation", source("ui.documentation.R", local = TRUE)$value)
        )
      )
    )
  ))


Server <- shinyServer(function(input, output, session) {
  source("server.database.R", local = TRUE)
  source("server.omicsnetwork.R", local = TRUE)
  source("server.integration.R", local = TRUE)
  source("server.functional_analysis.R", local = TRUE)
  source("server.Download.R", local = TRUE)
  source("server.home.R", local = TRUE)
})

shinyApp(UI, Server)

