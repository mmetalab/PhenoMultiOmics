
tags$head(
    tags$style(HTML("
        .shiny-input-panel {
          padding: 0px;
        }
        .sidebarPanel {
          padding: 0px;
        }
        .mainPanel {
          padding: 0px;
        }
        .box {
          margin-bottom: 20px;
        }
      "))
  )

tabPanel("Biomarker Discovery",
                  tabsetPanel(
                    tabPanel("Instruction",
                             fluidRow(
                               align = "center", 
                               column(12, 
                                      h1("Procedure to perform analysis of biomarker discovery", id = "fancyHeader"),
                                      fluidRow(
                                        div(class = "full-width-image",
                                            img(src = "biomarker1.png", height = "100px"), id = "biomarker1")
                                        
                                      ),
                                      fluidRow(
                                        div(class = "full-width-image",
                                            img(src = "biomarker2.png", height = "100px"), id = "biomarker2")
                                        
                                      ),
                                      fluidRow(
                                        div(class = "full-width-image",
                                            img(src = "biomarker3.png", height = "100px"), id = "biomarker3")
                                        
                                      )
                               )
                             )),
                    tabPanel("Statistical Analysis",
                             tabsetPanel(
                               tabPanel("Univariate Analysis",
                                        fluidRow(
                                          box(
                                            id="dynamicBox",
                                            title = "Upload data for differential omic expression analysis",
                                            width = 12,
                                            status = "primary",
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            fluidRow(
                                              column(4,
                                                     sidebarPanel(
                                                       width = 12,
                                                       tags$h4("Upload data"),
                                                       checkboxInput("sample_data3_1", "Upload the sample data", value = FALSE),
                                                       checkboxInput("own_data3_1", "Upload your own csv file", value = FALSE),
                                                       uiOutput("upload_options3_1"),
                                                       p("The column name is the gene name, and the row name is the sample name. The first column is the sample label."),
                                                       tags$hr(),
                                                       tags$h4("Univariate analysis"),
                                                       selectInput("colname3_1","Experimental group",choices  =c("treat")),
                                                       selectInput("colname3_2","Control group",choices  =c("control")),
                                                       selectizeInput("Pvalue3_1", "P value", choices = c("0.05","0.01")),
                                                       numericInput("logFC3_1", "LogFC value", value = 1),
                                                       shiny::actionButton("action3_1","Run",class = "btn-nm btn-danger")

                                                     )
                                              ),
                                              column(8,
                                                     mainPanel(
                                                       width = 12,
                                                       
                                                       uiOutput("conditionalText_table3_1"),
                                                       DT::dataTableOutput("table3_1"),
                                                       
                                                       uiOutput("conditionalText_table3_2"),
                                                       DT::dataTableOutput("table3_2"),
                                                       
                                                       verbatimTextOutput(outputId ="summary_input"),
                                                       downloadButton("download3_2","Download"),#,style = "display: none;"
                                                       collapsible = TRUE
                                                     )
                                              )
                                            )
                                          ),
                                          box(
                                          id="dynamicBox",
                                          title = "Volcano plot for univariate analysis",
                                          width = 12,
                                          status = "primary",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          fluidRow(
                                            column(4,
                                                   sidebarPanel(
                                                     width = 12,
                                                     p("Click the button generate volcano plot."),
                                                     shiny::actionButton("action3_2","Run",class = "btn-nm btn-danger")
                                                   )
                                            ),
                                            column(8,
                                                   mainPanel(
                                                     width = 12,
                                                     collapsible = TRUE,
                                                     plotOutput("plot3_1",height = "500px"),
                                                     tags$hr(),
                                                     tags$hr(),
                                                     downloadButton("download_plot3_1","Download"),
                                                     tags$hr(),
                                                     tags$hr()
                                                   )
                                            )
                                          )
                                          

                                        )
                               )

                             ),
                             tabPanel("PCA Analysis",
                              fluidRow(
                                box(
                                  id="dynamicBox",
                                  title = "Upload data for principal components analysis",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  fluidRow(
                                    column(4,
                                           sidebarPanel(
                                             width = 12,
                                             tags$h4("Upload data"),
                                             checkboxInput("sample_data4_1", "Upload the sample file data", value = FALSE),
                                             checkboxInput("own_data4_1", "Upload your own csv file", value = FALSE),
                                             uiOutput("upload_options4_1"),
                                             p("The column name is the variate name, and the row name is the sample name.The first column is the sample label."),
                                             tags$hr(),
                                             tags$h4("PCA analysis"),
                                             shiny::actionButton("action4_1","Run",class = "btn-nm btn-danger")
                                           
                                           )
                                    ),
                                    column(8,
                                           mainPanel(
                                             width = 12,
                                             collapsible = TRUE,
                                             tags$hr(),
                                             
                                             uiOutput("conditionalText_table4_1"),
                                             DT::dataTableOutput("table4_1"),
                                            
                                             uiOutput("conditionalText_table4_2"),
                                             DT::dataTableOutput("table4_2"),
                                             downloadButton("download4_1","Download")
                                           )
                                    )
                                  )
                                ),
                                box(
                                  id="dynamicBox",
                                  title = "PCA plot for principal components analysis",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  fluidRow(
                                    column(4,
                                           sidebarPanel(
                                             width = 12,
                                             # uiOutput("color_selectors"),
                                             shiny::actionButton("action4_2","Run",class = "btn-nm btn-danger")
                                           )
                                    ),
                                    column(8,
                                           mainPanel(
                                             width = 12,
                                             collapsible = TRUE,
                                             plotOutput("plot4_1",height = "500px"),
                                             tags$hr(),
                                             tags$hr(),
                                             downloadButton("download_plot4_1","Download"),
                                             tags$hr(),
                                             tags$hr()
                                           )
                                    )
                                  )
                                  
                                  
                                )
                              )
                              
                             ),
                             tabPanel("PLS-DA Analysis",
                              fluidRow(
                                box(
                                  id="dynamicBox",
                                  title = "Upload data for PLS-DA analysis",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  fluidRow(
                                    column(4,
                                           sidebarPanel(
                                             width = 12,
                                             tags$h4("Upload data"),
                                             checkboxInput("sample_data5_1", "Upload the sample file data", value = FALSE),
                                             checkboxInput("own_data5_1", "Upload your own csv file", value = FALSE),
                                             uiOutput("upload_options5_1"),
                                             p("The column name is the variate name, and the row name is the sample name.The first column is the sample label."),
                                             tags$hr(),
                                             tags$h4("PLS-DA analysis"),
                                             shiny::actionButton("action5_1","Run",class = "btn-nm btn-danger")
                                             
                                           )
                                    ),
                                    column(8,
                                           mainPanel(
                                             width = 12,
                                             collapsible = TRUE,
                                             tags$hr(),
                                            
                                             uiOutput("conditionalText_table5_1"),
                                             DT::dataTableOutput("table5_1"),
                                             
                                             uiOutput("conditionalText_table5_2"),
                                             DT::dataTableOutput("table5_2"),
                                             downloadButton("download5_1","Download")
                                           )
                                    )
                                  )
                                ),
                                box(
                                  id="dynamicBox",
                                  title = "PLS-DA plot for PLS-DA analysis",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  fluidRow(
                                    column(4,
                                           sidebarPanel(
                                             width = 12,
                                             # uiOutput("color_selectors2"),
                                             shiny::actionButton("action5_2","Run",class = "btn-nm btn-danger")
                                           )
                                    ),
                                    column(8,
                                           mainPanel(
                                             width = 12,
                                             collapsible = TRUE,
                                             plotOutput("plot5_1",height = "500px"),
                                             tags$hr(),
                                             tags$hr(),
                                             downloadButton("download_plot5_1","Download"),
                                             tags$hr(),
                                             tags$hr()
                                           )
                                    )
                                  )
                                  
                                  
                                )
                              )
                              
                             )
                             
                    )
                  ),
                  tabPanel("Functional Analysis",
                           fluidRow(
                             box(
                               id="dynamicBox",
                               title = "Upload data for functional analysis",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               fluidRow(
                                 column(4,
                                        sidebarPanel(
                                          width = 12,
                                          tags$h4("Upload data"),
                                          checkboxInput("sample_data6_1", "Upload the sample data", value = FALSE),
                                          checkboxInput("own_data6_1", "Upload your own csv file", value = FALSE),
                                          conditionalPanel(
                                            condition = "input.own_data6_1 == true",
                                            checkboxInput("gene_enrich", "Upload gene csv file", value = FALSE),
                                            checkboxInput("protein_enrich", "Upload protein csv file", value = FALSE),
                                            checkboxInput("metabolite_enrich", "Upload metabolite csv file", value = FALSE)
                                          ),
                                          conditionalPanel(
                                            condition = "input.protein_enrich == true",
                                            h5("Please upload Uniprot ID of protein, and the first column is 'Uniprot'.")
                                          ),
                                            
                                          uiOutput("upload_options6_1")
                                        )
                                 ),
                                 column(8,
                                        mainPanel(
                                          width = 12,
                                          collapsible = TRUE,
                                          tags$hr(),
                                          DT::dataTableOutput("table6_1")
                                        )
                                 )
                               )
                             ),
                             box(
                               id="dynamicBox",
                               title = "GO enrichment analysis",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               fluidRow(
                                 column(4,
                                        sidebarPanel(
                                          width = 12,
                                          tags$h4("Gene Ontology enrichment analysis"),
                                          selectInput("tab6_1_1","Select GO type",choices = c("BP","MF","CC","ALL")),
                                          selectInput("tab6_1_2","P AdjustMethod",choices = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr")),
                                          selectInput("tab6_1_3","P-value",choices  =c(0.01,0.05)),
                                          numericInput("tab6_1_4", "q value", value = 0.05),
                                          shiny::actionButton("tab6_1_5","Run",class = "btn-nm btn-danger")
                                        )
                                 ),
                                 column(8,
                                        mainPanel(
                                          width = 12,
                                          collapsible = TRUE,
                                          tags$hr(),
                                          verbatimTextOutput("progressText"), 
                                          DT::dataTableOutput("table6_1_1"),
                                          downloadButton("download_table6_1_1","Download table"),
                                          plotOutput("plot6_1_1", height = "500px", width = "100%"),
                                          downloadButton("download_plot6_1_1","Download plot")
                                        )
                                 )
                               )
                             ),
                             box(
                               id="dynamicBox",
                               title = "KEGG enrichment analysis",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               collapsible = TRUE,
                               fluidRow(
                                 column(4,
                                        sidebarPanel(
                                          width = 12,
                                          tags$h4("KEGG enrichment analysis"),
                                          # selectInput("tab6_2_1","Select key type",choices = c("kegg","ncbi-geneid","ncbi-proteinid","uniprot")),
                                          selectInput("tab6_2_2","P AdjustMethod",choices = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr")),
                                          selectInput("tab6_2_3","P-value",choices  =c(0.01,0.05)),
                                          numericInput("tab6_2_4", "q value", value = 0.05),
                                          shiny::actionButton("tab6_2_5","Run",class = "btn-nm btn-danger")
                                        )
                                 ),
                                 column(8,
                                        mainPanel(
                                          width = 12,
                                          collapsible = TRUE,
                                          tags$hr(),
                                          DT::dataTableOutput("table6_2_1"),
                                          downloadButton("download_table6_2_1","Download table"),
                                          plotOutput("plot6_2_1", height = "500px", width = "100%"),
                                          downloadButton("download_plot6_2_1","Download plot")
                                        )
                                 )
                               )
                             )
                             
                             
                             )
                 
                  
                  
                 
         ))
)