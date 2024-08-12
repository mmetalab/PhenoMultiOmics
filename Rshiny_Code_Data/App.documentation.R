library(shiny)
library(shinythemes)

UI <- shinyUI(tagList(
  navbarPage("PhenoMultiOmics",
             tags$head(
               # 引用 Font Awesome CDN
               tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.8.2/css/all.css"),
               tags$style(HTML("
      .shiny-code-container {
        padding: 15px;
        margin-bottom: 20px;
      }
      .icon {
        margin-right: 5px;
        color: #337ab7;
      }
    "))),
             theme = shinytheme("flatly"),
             source("ui.documentation.R",local = TRUE)$value
  ))
)

Server <- shinyServer(function(input,output,session){
  
})

shinyApp(UI,Server)
