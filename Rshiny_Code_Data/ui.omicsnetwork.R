# UI.omicsnetwork
tabPanel("MultiOmics Network",
tabsetPanel(
tabPanel("Upload file",
         sidebarPanel(
           selectInput("tab2_1_3","Disease",choices = c("Gastric cancer","Lung cancer","Esophagus cancer","Pancreatic cancer","Liver cancer",
                                                       "Intestinal cancer","Bladder cancer","Kidney cancer","Prostatic cancer","Metrocarcinoma",
                                                       "Throat cancer","Oral cancer","Skin cancer","Lymphoma cancer","Brain cancer","Breast cancer",
                                                       "Leukemia","Ovarian cancer","testicular cancer","Adrenal cancer"),
                       selected = NULL),
           shiny::actionButton("action2_2","Search",class = "btn-nm btn-danger"),
           hr(),
           fileInput("file3","Upload file",accept = c(".csv", ".txt")),
           helpText("Uploading the node file, which must contain a column named 'name'. The content can be Gene Symbol, EC number, or metabolite name."),
           radioButtons('sep3', "Separator", c(Comma=',',Tab='\t', Semicolon=';'))#,
         ),
         mainPanel(
           DT::dataTableOutput("table2_1"),
           DT::dataTableOutput("table2_2"),
           DT::dataTableOutput("table2_4"),
           dataTableOutput("table2_5"),
           downloadButton("download2_5","Download"),
           dataTableOutput("table2_6"),
           downloadButton("download2_6","Download")
         )),
tabPanel("MultiOmics Network",
         fluidRow(
           column(4, selectInput("tab2_1_2","Data",choices = c("Sample data","Upload nodes data"),selected ="Sample data")),
           column(4,selectInput("tab2_1_1","Layout",choices = c("layout_on_sphere","layout_on_grid","layout_nicely","layout_randomly",
                                                                "layout_in_circle","layout_with_fr","layout_with_sugiyama"),
                                selected = "layout_nicely"))
         ),
        
         
         sliderInput("range", "The Range of nodes size:",
                     min = 1, max = 50,
                     value = c(20,25)),
         shiny::actionButton("action2_1","Plot",class = "btn-nm btn-danger"),
         p("Click the button to update the multi-omics network displayed in the main panel."),
         fluidRow(column(12, div( visNetworkOutput("networkid1", height = "700px")))),
         DT::dataTableOutput("table2_3"),
         downloadButton("download2_1","Download"))
))
