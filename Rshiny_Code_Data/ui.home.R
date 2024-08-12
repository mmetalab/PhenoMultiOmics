################################home
library(shiny)



tabPanel("Home",
         fluidRow(
           align = "center", 
           column(12, 
                  h1("Welcome to PhenoMultiOmics", id = "fancyHeader"),
                  div(
                    id = "mainContent",
                    p("PhenoMultiOmics, an enzymatic reaction-based multi-omics web server, is designed to explore the scope of the multi-omics network across various cancer types. We first curated the PhenoMultiOmics Database (PMODB), which enables the retrieval of cancer-gene-protein-metabolite relationships based on the enzymatic reactions. 
                      We then developed the MultiOmics network visualization module to depict the interplay between genes, proteins, and metabolites in response to specific cancer-related enzymatic reactions. The biomarker discovery module facilitates functional analysis through differential omic feature expression and pathway enrichment analysis.",
                      class="text-justify")
                  )
           )
         ),
         fluidRow(
           div(class = "full-width-image",
               img(src = "construct.png", height = "100px"), id = "construct")
           
         )
)
