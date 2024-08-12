load("8.tsne.RData")

tabPanel("Multi-Omics Data Integration",
         tabsetPanel(
           tabPanel("Instruction",
                    fluidRow(
                      align = "center", 
                      column(12, 
                             h1("Procedure to perform Multi-Omics network", id = "fancyHeader"),
                             fluidRow(
                               div(class = "full-width-image",
                                   img(src = "integration1.png", height = "100px"), id = "integration1")
                               
                             ),
                             fluidRow(
                               div(class = "full-width-image",
                                   img(src = "integration2.png", height = "100px"), id = "integration2")
                               
                             ),
                             fluidRow(
                               div(class = "full-width-image",
                                   img(src = "integration3.png", height = "100px"), id = "integration3")
                               
                             )
                      )
                    )),
           tabPanel("Data processing",
                    fluidRow(
                      box(
                        title = "Upload Multi-Omics data",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(4,
                                 sidebarPanel(
                                   width = 12,
                                   tags$h4("Upload data"),
                                   checkboxInput("sample_integrat_data", "Upload the sample data", value = FALSE),
                                   checkboxInput("own_integrat_data", "Upload your own csv file", value = FALSE),
                                   uiOutput("upload_integrat_options")
                                 )
                          ),
                          column(8,
                                 mainPanel(
                                   width = 12,
                                   tags$hr(),
                                   
                                   uiOutput("conditionalText_table_gene"),
                                   DT::dataTableOutput("table_gene"),
                                  
                                   uiOutput("conditionalText_table_protein"),
                                   DT::dataTableOutput("table_protein"),
                                   
                                   uiOutput("conditionalText_table_metabolite"),
                                   DT::dataTableOutput("table_metabolite")
                                 )
                          )
                        )
                      ),
                      box(
                        title = "Data normalization",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(4,
                                 sidebarPanel(
                                   width = 12,
                                   pickerInput("data_choice", "Select data type", choices = c("All", "Gene", "Protein", "Metabolite"), multiple = FALSE),
                                   selectInput("norm_method","Run Normalization method",choices = c("Min-Max","Z-Score")),
                                   shiny::actionButton("Action_Normalization", "Normalization", class = "btn-nm btn-danger")
                                 )
                          ),
                          column(8,
                                 mainPanel(
                                   width = 12,
                                   DTOutput("table_normalization"),
                                   conditionalPanel(
                                     condition = "input.data_choice == 'All'",
                                     DTOutput("table_normalization_gene"),
                                     DTOutput("table_normalization_protein"),
                                     DTOutput("table_normalization_metabolite")
                                   ),
                                   downloadButton("download_table_normalization","Download")
                                 )
                          )
                        )
                      )
                    )
             
           ),
           tabPanel("Multi-Omics data integration",
                    fluidRow(
                      box(
                        title = "t-SNE Analysis",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(4,
                                 sidebarPanel(
                                   width = 12,
                                   selectInput("data_select","Select data",choices = c("Normalization data","Row data")),
                                   shiny::actionButton("action_tsne", "Run t-SNE", class = "btn-nm btn-danger")
                                 )
                          ),
                          column(8,
                                 mainPanel(
                                   width = 12,
                                   # DT::dataTableOutput("tsne_Data"),
                                   plotOutput("tsne_plot", height = "500px"),
                                   downloadButton("download_tsne_plot","Download")
                                 )
                          )
                        )
                      ),
           box(
             title = "UMAP Analysis",
             width = 12,
             status = "primary",
             solidHeader = TRUE,
             collapsible = TRUE,
             fluidRow(
               column(4,
                      sidebarPanel(
                        width = 12,
                        selectInput("data_select2","Select data",choices = c("Normalization data","Row data")),
                        shiny::actionButton("action_umap", "Run UMAP", class = "btn-nm btn-danger")
                      )
               ),
               column(8,
                      mainPanel(
                        width = 12,
                        plotOutput("umap_plot", height = "500px"),
                        downloadButton("download_umap_plot","Download")
                      )
               )
               
             )
           )
           ))
))