load("3.DE_analysis.RData")
load("3_1.Volcano.RData")
load("4.diff.RData")

tabPanel("Functional analysis",
         tabsetPanel(
           tabPanel("DE analysis",
                    tabsetPanel(
                      tabPanel("Upload data",
                               sidebarPanel(
                                 fileInput("file3_1","Upload gene file",accept =  c(".csv", ".txt")),
                                 radioButtons('sep', "Separator", c(Comma=',',Tab='\t', Semicolon=';')),
                                 p("The column name is the sample name, the row name is the gene name"),
                                 tags$hr(),
                                 fileInput("file3_2","Upload group of sample",accept =  c(".csv", ".txt")),
                                 radioButtons('sep', "Separator", c(Comma=',',Tab='\t', Semicolon=';')),
                                 p("A column of group was included, and the experimental group was 'treat' and the control group was 'control' "),
                                 tags$hr()),
                               mainPanel(
                                 DT::dataTableOutput("table3_1"),
                                 DT::dataTableOutput("table3_2"),
                               )),
                      tabPanel("DE analysis",
                               sidebarPanel(width = 3,
                                            selectInput("colname3_1","experimental group",choices  =c("treat")),
                                            selectInput("colname3_2","control group",choices  =c("control")),
                                            selectizeInput("Pvalue3_1", "P value", choices = c("0.05","0.01")),
                                            numericInput("logFC3_1", "logFC value", value = 1),
                                            shiny::actionButton("action3_1","calculate",class = "btn-nm btn-danger"),
                                            p("compute the Differential expression gene"),
                                            tags$hr()),
                               mainPanel(
                                 DT::dataTableOutput("table3_3"),
                                 downloadButton("download3_3","Download"),
                                 DT::dataTableOutput("table3_4"),
                                 verbatimTextOutput(outputId ="summary_input"),collapsible = TRUE)
                      ),
                      tabPanel("Volcano plot",
                               sidebarPanel(
                                 fileInput("file3_3","upload gene file",accept = c(".csv", ".txt")),
                                 p("The file contain the column of logFC and P"),
                                 shiny::actionButton("action3_3","plot",class = "btn-nm btn-danger"),
                                 tags$hr()),
                               mainPanel(
                                 plotOutput("plot3_3",height = "500px")#,
                                 # downloadButton("download_p3_3","Download")
                               ))
                    )),
           tabPanel("GSP Analysis",
                    tabsetPanel(
                      tabPanel("upload data",
                               sidebarLayout(
                                 sidebarPanel(
                                   fileInput("file4_1","upload gene file",accept = c(".csv", ".txt")),
                                   radioButtons('sep', "分隔符", c(Comma=',',Tab='\t', Semicolon=';')),
                                   tags$hr(),
                                   selectInput("P4_1","P-value",choices  =c(0.01,0.05)),
                                   numericInput("q4_1", "q value", value = 0.05),
                                   selectInput("j4_1","adjust method",choices  =c("holm","hochberg","hommel","bonferroni","BH","BY", "fdr")),
                                   shiny::actionButton("action4_1","calculate",class = "btn-nm btn-danger"),
                                 ),
                                 mainPanel(DT::dataTableOutput("table4_1"),
                                           DT::dataTableOutput("table4_2"))
                               )),
                      tabPanel("GSP Analysis",
                               sidebarLayout(
                                 sidebarPanel(
                                   numericInput("number4_1", "number", value = 10),
                                   shiny::actionButton("action4_2","calculate",class = "btn-nm btn-danger"),
                                   tags$hr()
                                 ),
                                 mainPanel(plotOutput("plot4_1"),
                                           plotOutput("plot4_2"))
                               )),
                      tabPanel("KEGG network",
                               sidebarLayout(
                                 sidebarPanel(
                                   # width = 3,
                                   numericInput("number4_3", "number", value = 4),
                                   selectInput("P4_3","Layout",
                                               choices  =c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 'randomly', 'fr', 'kk', 'drl','lgl'),
                                               selected = 'graphopt'),
                                   shiny::actionButton("action4_3","calculate",class = "btn-nm btn-danger"),
                                   tags$hr()
                                 ),
                                 mainPanel(plotOutput("plot4_3"))
                               ))
                      
                    )  
           )
         )
)
