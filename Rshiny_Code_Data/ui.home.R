################################home
library(shiny)



tabPanel("Home",
         fluidRow(
           align = "center", 
           column(12, 
                  h1("Welcome to PhenoMultiOmics", id = "fancyHeader"),
                  div(
                    id = "mainContent",
                    p("PhenoMultiOmics, an enzymatic reaction-based multi-omics web server designed to explore the scope of the multi-omics network across various cancer types. We first curated the PhenoMultiOmics Database (PMODB), which enables the retrieval of cancer-gene-protein-metabolite relationships based on the enzymatic reactions. We then developed the MultiOmics network visualization module to depict the interplay between genes, proteins, and metabolites in response to specific cancer-related enzymatic reactions. The biomarker discovery module facilitates functional analysis through differential omic feature expression and pathway enrichment analysis. PhenoMultiOmics has been applied to analyze transcriptomics data of gastric cancer and metabolomics data of lung cancer, providing insights into interrupted enzymatic reactions and the associated multi-omics network.",
                      class="justify-text")
                  )
           )
         ),
         fluidRow(
           div(class = "full-width-image",
               img(src = "construct.png", height = "100px"), id = "construct")
           
         ),
         fluidRow(
           align = "center", 
           column(12, 
                  div(
                    id = "mainContent2",
                    p("This website contains data on enzymatic reaction-based multi-omics for 20 types of tumors, including leukemia, bladder cancer, intestinal cancer, lung cancer, liver cancer, testicular cancer, laryngeal cancer, oral cancer, lymphoma, ovarian cancer, brain cancer, skin cancer, prostate cancer, breast cancer, kidney cancer, adrenal cancer, esophageal cancer, pancreatic cancer, uterine cancer, and stomach cancer.",
                      class="justify-text")
           )
         )),
         fluidRow(
           div(class = "full-width-image",
               img(src = "tumor.png", height = "100px"), id = "tumor")
           
         )
)
