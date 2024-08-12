# UI.database
library(shiny)
load("1_2.feature_met.RData")
load("1_3.feature_pro.RData")

#################
tabPanel("Multi-Omics Database",
tabsetPanel(
  tabPanel("Instruction",
           fluidRow(
             align = "center", 
             column(12, 
                    h1("Workflow for constructing the Multi-Omics database", id = "fancyHeader"),
             )
           ),
           fluidRow(
             div(class = "full-width-image",
                 img(src = "Workflowplot.png", height = "100px"), id = "Workflowplot")
             
           ),
           fluidRow(
             div(class = "full-width-image",
                 img(src = "Workflowplot_2.png", height = "100px"), id = "Workflowplot")
             
           )
           ),
  tabPanel("PMODB",
           br(),
           helpText("The PMODB database gathered enzymatic reactions from multiple enzymatic reaction databases, including Metabolic Atlas, BRENDA database, RHEA database, and EnzymeMap database (Li et al., 2023; Wang et al., 2021; Schomburg, 2004; Bansal et al., 2022; Heid et al., 2023). 
                    For metabolites and proteins that involved in the same enzymatic reaction, it is regarded as protein-metabolite association. To generate the connection between genes, proteins, metabolites, and diseases, we then mapped the enzymes/proteins to associated genes using UniProt Knowledgebase, followed by generating the gene-disease association information based on the Disgenet database (McGarvey et al., 2019; The UniProt Consortium et al., 2021; Piñero et al., 2019)."),
           hr(),
           selectInput("tab1_4","Select disease:",
                       sort(c("breast cancer", "prostate cancer","liver cancer", "stomach cancer","colon cancer","kidney cancer","ovarian cancer","esophageal cancer","pancreatic cancer","urinary bladder cancer","uterine cancer", "bone cancer","thyroid cancer","head and neck cancer","brain cancer",
                                                         "cervical cancer","skin cancer","larynx cancer","bile duct cancer","germ cell cancer","adrenal gland cancer", "ocular cancer","muscle cancer","testicular cancer","rectum cancer","gallbladder cancer","salivary gland cancer","thymus cancer","pharynx cancer",
                                                         "vulva cancer","lymphatic system cancer","peritoneum cancer","duodenum cancer","appendix cancer","spinal cancer","penile cancer","vascular cancer","vaginal cancer", "fallopian tube cancer", "tracheal cancer","mediastinal cancer","retroperitoneal cancer","ureter cancer")),
                       selected="stomach cancer"),
           DTOutput("table1"),
           hr(),
           downloadButton("download1","Download")
  ),
  tabPanel("Enzyme DB",
           br(),
           helpText("The Enzyme DB contains information of all enzymes in PMODB, including enzyme name, Uniprot ID, EC number, etc."),
           # helpText("You can match more information about proteins in the Uniprot database based on the uniprot ID."),
           DTOutput("table1_2"),
           hr(),
           downloadButton("download1_2","Download"),
           tags$head(
             tags$style(HTML("
                    .chart-container { height: 400px; }
                "))
           ),
           fluidRow(
             align = "center", 
             column(12, 
                    h3("Distribution of Protein Categories"))
           ),
           
           # 饼图 image2_1
           fluidRow(
             column(width = 12, 
                    plotOutput("image2_1", height = "400px"))
           ),
           
           fluidRow(
             align = "center", 
             column(12, 
                    h3("Bar plot of the number of enzymatic reactions in different cancer types in PMODB.", id = "fancyHeader"))
           ),
           fluidRow(
             div(class = "full-width-image",
                 img(src = "database1.png", height = "100px"), id = "Workflowplot")
             
           ),
           fluidRow(
             align = "center", 
             column(12, 
                    h3("Bar plot of the number of gene-protein-metabolite associations in different cancer types in PMODB.", id = "fancyHeader"))
           ),
           fluidRow(
             div(class = "full-width-image",
                 img(src = "database2.png", height = "100px"), id = "Workflowplot")
             
           )
           ),
  
  tabPanel("Metabolite DB",
           br(),
           helpText("The Metabolite DB contains information of all metabolites in PMODB, including metabolite name, HMDB ID, SMILES, chemical formula, etc."),
          DT::dataTableOutput("table1_1"),
           hr(),
           downloadButton("download1_1","Download"))

  )
)