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
# library(maps)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
# library(FactoMineR)
# library(factoextra)

library(clusterProfiler)
library(pathview)
# library(topGO)
library(ggpubr)
library(dashboardthemes)
library(shinythemes)

library(limma)
library(pathview)
library(clusterProfiler)
library("org.Hs.eg.db")

custom_css <- HTML("
<link href='https://fonts.googleapis.com/css?family=Open+Sans:400,700' rel='stylesheet'>
<style>
body {
  font-family: 'Open Sans', sans-serif;
}

#fancyHeader {
  font-weight: 700; /* 加粗字体 */
  /* 其他样式保持不变 */
}

.full-width-image img {
  width: calc(100% - 72px); /* 减去左右边距 */
  height: auto; /* 高度自动调整以保持长宽比 */
  margin: 36px; /* 上下左右各36px边距 */
}

.full-width-image {
  padding-left: 36px; /* 左边距 */
  padding-right: 36px; /* 右边距 */
}

/* 您可以在这里添加其他自定义CSS样式 */
</style>
")


UI <- shinyUI(
  tagList(
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        shinyDashboardThemes(theme = "poor_mans_flatly"),
        navbarPage("PhenoMultiOmics",
                   tags$head(
                     custom_css,
                     # 引用 Font Awesome CDN
                     tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.8.2/css/all.css"),
                     tags$style(HTML("
                     .shiny-code-container {padding: 15px;margin-bottom: 20px;}
                     .icon {margin-right: 5px;color: #337ab7; }
                     .justify-text {
            text-align: justify;
            text-justify: inter-word;
        }"))),
                 theme = shinytheme("flatly"),
                 source("ui.home.R",local = TRUE)$value,
                 source("ui.database.R",local = TRUE)$value,
                 source("ui.omicsnetwork.R",local = TRUE)$value,
                 source("ui.functional_analysis.R",local = TRUE)$value,
                 source("ui.tutorial.R",local = TRUE)$value,
                 source("ui.documentation.R",local = TRUE)$value
                 
                 )
      )
    )
  ))

Server <- shinyServer(function(input,output,session){
  source("server.database.R",local = TRUE)
  source("server.omicsnetwork.R",local = TRUE)
  source("server.functional_analysis.R",local = TRUE)
  source("server.home.R",local = TRUE)
  }
  )

shinyApp(UI,Server)

