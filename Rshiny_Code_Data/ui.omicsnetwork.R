load("2.omicsnetwork.RData")
############################
library(shiny)
library(shinydashboard)
library(DT)
library(visNetwork)
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

tabPanel("Multi-Omics Network",
         tabsetPanel(
           tabPanel("Instruction",
                    fluidRow(
                      align = "center", 
                      column(12, 
                             h1("Procedure to perform multi-omics network", id = "fancyHeader"),
                             fluidRow(
                               div(class = "full-width-image",
                                   img(src = "network1.png", height = "100px"), id = "network1")
                               
                             ),
                             fluidRow(
                               div(class = "full-width-image",
                                   img(src = "network2.png", height = "100px"), id = "network2")
                               
                             ),
                             fluidRow(
                               div(class = "full-width-image",
                                   img(src = "network3.png", height = "100px"), id = "network3")
                               
                             )
                      )
                    )),
           tabPanel("Multi-Omics Network",
                    fluidRow(
                      box(
                        title = "Upload data to generate multi-omics network visualization",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(4,
                                 sidebarPanel(
                                   width = 12,
                                   selectInput("tab2_1_3","Select disease",choices =  c("Please select disease" = "", 
                                                                                 sort(c("breast cancer", "prostate cancer","liver cancer", "stomach cancer","colon cancer","kidney cancer","ovarian cancer","esophageal cancer","pancreatic cancer","urinary bladder cancer","uterine cancer", "bone cancer","thyroid cancer","head and neck cancer","brain cancer",
                                                                                        "cervical cancer","skin cancer","larynx cancer","bile duct cancer","germ cell cancer","adrenal gland cancer", "ocular cancer","muscle cancer","testicular cancer","rectum cancer","gallbladder cancer","salivary gland cancer","thymus cancer","pharynx cancer",
                                                                                        "vulva cancer","lymphatic system cancer","peritoneum cancer","duodenum cancer","appendix cancer","spinal cancer","penile cancer","vascular cancer","vaginal cancer", "fallopian tube cancer", "tracheal cancer","mediastinal cancer","retroperitoneal cancer","ureter cancer"))),
                                               selected = NULL),
                                   shiny::actionButton("action2_2","Confirm",class = "btn-nm btn-danger"),
                                   hr(),
                                   helpText("Uploading the node file, the first column must be named 'Name'. The content can be Gene Symbol, Enzyme name, or metabolite name. Please select and confirm the disease before uploading data."),
                                   checkboxInput("sample_data", "Upload the sample data", value = FALSE),
                                   checkboxInput("own_data", "Upload your own csv file", value = FALSE),
                                   uiOutput("upload_options"),
                                   shiny::actionButton("action2_3","Generate",class = "btn-nm btn-danger"),
                                   helpText("This button is to generate the node and edge files for Multi-Omics network.")
                                 )
                          ),
                          column(8,
                                 mainPanel(
                                   width = 12,
                                   uiOutput("conditionalText2_4"),
                                   DT::dataTableOutput("table2_4"),
                                   
                                   uiOutput("conditionalText2_6"),
                                   dataTableOutput("table2_6"),
                                  
                                   uiOutput("conditionalText2_5"),
                                   dataTableOutput("table2_5"),
                                   
                                   uiOutput("downloadButtonUI")
                                 )
                          )
                        )
                      ),
                      box(
                        title = "Plot of Multi-Omics network",
                        width = 12,
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        fluidRow(
                          column(4,
                                 sidebarPanel(
                                   width = 12,
                                   selectInput("tab2_1_1","Layout",choices = c("layout_on_sphere","layout_on_grid","layout_nicely","layout_randomly",
                                                                               "layout_in_circle","layout_with_fr","layout_with_sugiyama"),
                                               selected = "layout_on_sphere"),
                                   selectInput("tab2_1_2","Node size",choices = c("Abundance","Degree"),
                                               selected = "Degree"),
                                   sliderInput("range", "The range of nodes size:",
                                               min = 1, max = 50,
                                               value = c(20,25)),
                                   shiny::actionButton("action2_1","Plot",class = "btn-nm btn-danger"),
                                   p("Click the button to update the Multi-Omics network displayed On the main panel.")
                                 )
                          ),
                          column(8,
                                 mainPanel(
                                   width = 12,
                                   fluidRow(
                                     align = "center", 
                                     column(12, 
                                            h3("The node size attribute for the Multi-Omics network is shown below.", class = "light-gray-text"))
                                   ),
                                   DT::dataTableOutput("table2_3"),
                                   downloadButton("download2_1","Download"),
                                   useShinyjs(),
                                   div(visNetworkOutput("networkid1", height = "700px")),
                                   uiOutput("networkStats"),
                                   actionButton("action2_1", "Update Network")
                                   
                                 )
                          )
                        )
                      )
                    )
           )
         )
)
