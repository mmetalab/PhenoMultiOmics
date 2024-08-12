library(shiny)
library(shinythemes)

library(shiny)

# 自定义的CSS样式
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



UI <- shinyUI(tagList(
  navbarPage("PhenoMultiOmics",
             theme = shinytheme("flatly"),
             tags$head(custom_css),
             source("ui.home.R",local = TRUE)$value
  ))
)

Server <- shinyServer(function(input,output,session){
  source("server.home.R",local = TRUE)}
)

shinyApp(UI,Server)