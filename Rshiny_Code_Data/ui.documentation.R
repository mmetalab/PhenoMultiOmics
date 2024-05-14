##############UI.introduction
library(shiny)
library(shinythemes)
library(shinydashboard)




Database_of_GC<- "PhenoMultiOmics database (PMODB) mines genes associated with different cancers and their enzymatic reaction metabolism based on the DisGeNET and Metabolic Atlas databases. It also associates protein data through the UniProt database, forming a multi-omics database of disease-gene-protein-metabolite. 
It comprises essential information about diseases, genes, proteins, and metabolites, each assigned a public database. For genes and diseases, it includes the disease name and ID, gene name, gene symbol, and gene-disease association score. Regarding proteins, the database contains information such as protein name, enzyme classification number (EC number), UniProt ID, enzyme name, protein sequence length, and the corresponding gene name for the protein. As for metabolites, PMODB includes metabolite name, chemical formula, SMILES notation, and associated metabolic pathways.
PMODB enables rapid access to multi-omics reaction relationship information by querying genes, proteins, or metabolites, with the query results available for download in tabular format."

Database_of_GC_split <- gsub("PMODB enables rapid access", "<br><br>PMODB enables rapid access", Database_of_GC)


Omics_network <- "Based on the established PhenoMultiOmics Database (PMODB), a multi-omics network is generated utilizing enzymatic reaction data involving genes, proteins, and metabolites. In this network, each gene, protein, or metabolite is represented as a node. The enzymatic reaction information is used to create edges between genes, proteins, and metabolites. Specifically, in enzymatic reactions, edges denote interactions between proteins (enzymes) and metabolites (substrates and products). Additionally, for genes that regulate protein expression, connections are established between genes and proteins, as well as between genes and metabolites.
This section is divided into two modules, one for uploading data and the other for generating MultiOmics networks. When uploading data, first select the type of disease, involving 20 types of tumors. After clicking search, the main dashboard will display all the node file and edge file of the tumor. After uploading the file, the main dashboard automatically generates tumor-related enzymatic reactions and multi-omics associations extracted from the node file.

In the module dedicated to generating MultiOmics networks, you have the ability to visualize instance data networks. This module also allows you to select uploaded data as Nodes Data. By clicking 'plot', you can create intricate multi-omics graphs. To enhance the aesthetic appeal of these network diagrams, you can adjust the layout style and vary the range of node sizes, adding an extra layer of customization and clarity to your data visualization."

Omics_network_split <- gsub("This section is divided into two modules,", "<br><br> This section is divided into two modules,", Omics_network)
Omics_network_split2 <- gsub("In the module dedicated to generating MultiOmics", "<br><br> In the module dedicated to generating MultiOmics", Omics_network)


Gene_expression <- "For statistical analysis, differential omic feature data analysis is embedded, which require the matrices of gene expression, proteomics, or metabolomics data as input. Each row of this matrix represents a gene or feature, and each column corresponds to a sample ID. This analysis leverages the lima R package to calculate the Log2 Fold Change (Log2FC), estimating differences between case and control groups .Results are presented in tables and volcano plot."
Gene_set_enrichment_analysis <- "Furthermore, for differentially expressed genes, Gene Set Pathway Enrichment Analysis (GSEA) is conducted, drawing from the clusterProfiler package . This analysis utilizes human genome annotation from org.Hs.eg.db and pathway enrichment data from the KEGG database. Enrichment factors and BH-corrected P-values are computed to identify significant enrichment pathways, displayed via bar and bubble charts. Finally, the enrichplot R package is used to visualize networks of individual genes and pathways."

Copyright <- HTML(
  "<p>PhenoMultiOmics is offered to the public as a freely available resource.</p>",
  "<p>Use and re-distribution of the data, in whole or in part, for commercial purposes requires explicit permission of the authors and explicit acknowledgment of the source material (PhenoMultiOmics) and the original publication.</p>",
  "<p>We ask that users who download significant portions of the database cite the PhenoMultiOmics paper in any resulting publications.</p>",
  "<p>For commercial licences, please consult with chengwangsdu@outlook.com.</p>"
  
)



tabPanel(
  "Documentation",
   fluidRow(
    column(12,
           tags$h3(tags$span(class="fas fa-database icon"), "MultiOmics Database"),
           div(class="container-fluid shiny-code-container well", HTML(Database_of_GC_split)),
           
           tags$h3(tags$span(class="fas fa-network-wired icon"), "MultiOmics Network"),
           div(class="container-fluid shiny-code-container well", HTML(Omics_network_split, Omics_network_split2)),
           
           tags$h3(tags$span(class="fas fa-chart-bar icon"), "Functional Analysis"),
           div(class="container-fluid shiny-code-container well", HTML(Gene_expression), br(), br(), HTML(Gene_set_enrichment_analysis)),
           
           tags$h3(tags$span(class="fas fa-copyright icon"), "Citing the PhenoMultiOmics"),
           div(class="container-fluid shiny-code-container well", HTML(Copyright))
           
           
  ))
)
