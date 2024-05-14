# UI.database
library(shiny)
load("1_1.database.RData")
load("1_2.feature_met.RData")
load("1_3.feature_pro.RData")

#################
tabPanel("MultiOmics DB",
tabsetPanel(
  tabPanel("Protein DB",
           br(),
           helpText("This is a table of protein information, including Uniprot ID, EC number, Gene Name(s) and synonym(s) of the protein, Length of the canonical sequence, KEGG cross-reference."),
           helpText("You can match more information about proteins in the Uniprot database based on the uniprot ID."),
           DT::dataTableOutput("table1_2"),
           hr(),
           downloadButton("download1_2","Download"),
           # # fluidRow(
           # #   column(6, 
           #          plotOutput("image2_1"),#),  # 左侧显示 image2_1
           #   # column(6, 
           #          plotOutput("image2_2")#))
           # 图表容器的样式
           tags$head(
             tags$style(HTML("
                    .chart-container { height: 400px; }
                "))
           ),
           
           # 饼图 image2_1
           fluidRow(
             column(width = 12, 
                    plotOutput("image2_1", height = "400px"))
           ),
           
           # 条形图 image2_2
           fluidRow(
             column(width = 12, 
                    plotOutput("image2_2", height = "400px"))
           )
           
           ),
  
  tabPanel("Metabolic reaction",
           br(),
           helpText("The data of this database come from disgenet and Metabolic Atlas, 
                    and the reaction network of gene-protein-metabolite is established, and the metabolic ID is used as the metabolic reaction of each gene.
                    You can search for related metabolic reactions based on genes, proteins or metabolites."),
           hr(),
           selectInput("tab1_4","Disease:",c("Gastric cancer","Lung cancer","Esophagus cancer","Pancreatic cancer","Liver cancer",
                                             "Intestinal cancer","Bladder cancer","Kidney cancer","Prostatic cancer","Metrocarcinoma",
                                             "Throat cancer","Oral cancer","Skin cancer","Lymphoma cancer","Brain cancer","Breast cancer",
                                             "Leukemia","Ovarian cancer","testicular cancer","Adrenal cancer")),
           
           column(4,
                  selectInput("tab1_1", "Gene:", "All")
                  ),
           column(4,
                  selectInput("tab1_2", "Protein:", "All")
                 ),
           column(4,
                  selectInput("tab1_3", "Metabolite:", "All")
                  ),
           DT::dataTableOutput("table1"),
           hr(),
           downloadButton("download1","Download")
           ),
  tabPanel("Metabolite DB",
           br(),
           helpText("The following table is the database of metabolites, including HMDB_ID,SMILE,chemical formulate and pathwayID information."),
          DT::dataTableOutput("table1_1"),
           hr(),
           downloadButton("download1_1","Download"))

  )
)

