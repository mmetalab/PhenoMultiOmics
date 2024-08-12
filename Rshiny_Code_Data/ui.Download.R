##############UI.tutorial
library(shiny)
library(shinythemes)
library(shinydashboard)

tabPanel(
  "Download",
  fluidRow(
    column(12,
           
           tags$h3(tags$span(class="fas fa-file-csv icon"), "Multi-Omics Network"),
           div(class="container-fluid shiny-code-container well",   
               class = "justify-text",
               HTML("Sample data for generating Multi-Omics network")),
           downloadButton( "Download_sample_data1","Download")
           
    ),
    column(12,
           
           tags$h3(tags$span(class="fas fa-file-csv icon"), "Multi-Omics Data Integration"),
           div(class="container-fluid shiny-code-container well",   
               class = "justify-text",
               HTML("Sample data of Multi-Omics Data Integration")),
           downloadButton( "Download_sample_data2","Download")
           
    ),
    column(12,
           
           tags$h3(tags$span(class="fas fa-file-csv icon"), "Biomarker Discovery"),
           div(class="container-fluid shiny-code-container well",   
               class = "justify-text",
               HTML("Sample data of univariate analysis for statistical analysis")),
           downloadButton( "Download_sample_data3","Download"),
           
           div(class="container-fluid shiny-code-container well",   
               class = "justify-text",
               HTML("Sample data of PCA and PLS-DA analysis")),
           
           downloadButton( "Download_sample_data4","Download"),
           
           div(class="container-fluid shiny-code-container well",   
               class = "justify-text",
               HTML("Sample data for functional analysis")),
           
           downloadButton( "Download_sample_data5","Download"),
    )
    
    )
)

