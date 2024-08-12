##############UI.introduction
library(shiny)
library(shinythemes)
library(shinydashboard)

PhenoMultiOmics_database <- HTML("To construct the enzymatic reaction inferred multi-omics database, we extracted the enzymatic reactions from multiple enzymatic reaction databases, including Metabolic Atlas, BRENDA database, RHEA database, and EnzymeMap database (Li et al., 2023; Wang et al., 2021; Schomburg, 2004; Bansal et al., 2022; Heid et al., 2023). Each enzymatic reaction is denoted as a chemical reaction equation with reactants and products, including the enzymes involved in the reaction. 
To generate the connection between genes, proteins, metabolites, and diseases, we first mapped the enzymes/proteins to associated genes using Uniprot database, followed by generating the gene-disease association information based on the Disgenet database. In this study, we focused on the top twenty common cancer types. 
For metabolites and proteins that involved in the same enzymatic reaction, it is regarded as protein-metabolite association. Therefore, the PhenoMultiOmics database (PMODB) includes 5,540 enzymatic reactions across 45 cancer types, which incorporate 759,558 gene-protein-metabolite-disease connections. 
PMODB also comprises essential information about diseases, genes, proteins, and metabolites with link to external databases. For genes and diseases, it includes the disease name and ID, gene name, gene symbol, and gene-disease association score. Regarding proteins, the database contains information such as protein name, enzyme classification (EC) number, UniProt ID, enzyme name, protein sequence length, and the corresponding gene name for the protein. As for metabolites, PMODB includes metabolite name, chemical formula, SMILES notation, and associated metabolic pathways.")

Multi_omic_data_integration_module <- HTML("The multi-omic data integration module accepts input of preprocessed multi-omic dataset., including transcriptomics, proteomics, and metabolomics. The module first generates the visualization of these uploaded dataset for visual inspection by t-SNE and UMAP, which aims to observe the distribution of the abundance of genes, proteins, and metabolites. Users have options to select which type of omics dataset for subsequent biomarker discovery analysis. After select one or multiple omic dataset, the module will conduct normalization to unify the scale of different omic dataset. Then, the dimensionality reduction and visualization will be performed to further inspect the normalized and integrated multi-omic data.")

Multi_omics_network_visualization <- HTML("Based on the established PMODB, a multi-omics network is generated utilizing enzymatic reaction data involving genes, proteins, and metabolites. In this network, each gene, protein, or metabolite is represented as a node. The enzymatic reaction information is used to create edges between genes, proteins, and metabolites. Specifically, in enzymatic reactions, edges denote interactions between proteins (enzymes) and metabolites (substrates and products). Additionally, for genes that regulate protein expression, connections are established between genes and proteins, as well as between genes and metabolites. Considering the bidirectional or reversible nature of enzymatic reactions, the network does not contain the direction of reactions, and edge weights are not assigned (Pogodaev et al., 2019).")

Biomarker_discovery_module <- HTML("The PhenoMultiOmics web server incorporates a biomarker discovery module for statistical and functional analysis to discover the key differentially expressed multi-omic biomarkers in case-control studies. For statistical analysis, the univariate and multivariate analysis were embedded. Since the PMODB was designed based on enzymatic reactions with gene, protein, and metabolite information, the biomarker discovery module accepts the preprocessed transcriptomics, proteomics, and metabolomics dataset acquired from case-control cohorts. Specifically, it requires the matrices of gene expression, proteomics, or metabolomics data as input. Each row of this matrix represents a quantitative abundance value of a sample, and each column corresponds to a feature. The univariate analysis performs significance testing to detect differences between case and control groups and assess the statistical significance of these differences. For statistical test, Benjamini and Hochberg (BH) correction could be applied to adjust the P-value if necessary. Results are presented in tables and volcano plots, with default criteria for significance level of P-value < 0.05 and an absolute Log2 Fold Change (Log2FC) ≥ 1. Multivariate analysis includes principal component analysis (PCA) and partial least squares discriminant analysis (PLS-DA) that performs dimensionality reduction and feature selection. Functional analysis using omics data aims to understand the biological functions and pathways associated with the genes, proteins, or metabolites identified in an omics study. This analysis facilitates the interpretation of the biological significance of the results, uncovering the underlying mechanisms of diseases or phenotypes. Gene Ontology Enrichment Analysis (GOEA) and Metabolic Pathway Analysis (MPA) are embedded on the web server. GOEA identifies which GO terms (biological processes, cellular components, and molecular functions) are overrepresented given a set of genes or proteins. MPA identifies the metabolic pathways that are significantly affected in a metabolomics dataset. For the enrichment analysis, enrichment factors and BH-corrected P-values are computed to identify significant enrichment pathways, displayed via bar and bubble charts. Finally, the enrichplot R package is used to visualize networks of individual genes and pathways.")

Implementation_in_R <- HTML("The PhenoMultiOmics web server is programmed in R (version: 4.4.1) and relies on packages provided by Bioconductor and CRAN. The multi-omics network is constructed using the visNetwork package, while the limma package facilitates statistical analysis. All visualizations, including figures and plots presented on the web server, are created with the ggplot2 package and are available for download (Ito and Murphy, 2013; Ritchie et al., 2015; Yu et al., 2012). The source code and processed data used in the manuscript is deposited on Github https://github.com/mmetalab/PhenoMultiOmics.")

Citing_PhenoMultiOmics <- HTML('PhenoMultiOmics is licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0) license. This license grants users the freedom to:</p>',
                              
                               '<li>Distribute: Share copies of the work in any medium or format.',
                               
                               '<li>Remix: Adapt, transform, and build upon the work.',
                               '<li>Commercial Use: Utilize the work for commercial purposes.</p>',
                               'Under this license, users must comply with the following terms:',
                               '<li> Attribution: Users must give appropriate credit, provide a link to the license, and indicate if changes were made. Credit must be given in a reasonable manner, but not in a way that suggests the licensor endorses the user or their use.',
                               '<li>No Additional Restrictions: Users may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.</p>',
                               'For further questions, please contact chengwangsdu@outlook.com.</p>'
                               )
tabPanel(
  "Documentation",
  fluidRow(
    column(12,
           tags$h3(tags$span(class="fas fa-database icon"),
                   "PhenoMultiOmics database"),
           div(class="container-fluid shiny-code-container well",
               class = "justify-text",
               HTML(PhenoMultiOmics_database)
           ),
           
           tags$h3(tags$span(class="fas fa-network-wired icon"),
                   "Multi-Omics network visualization"),
           div(class="container-fluid shiny-code-container well",
               class = "justify-text",
               HTML(Multi_omics_network_visualization)
           ),
           
           tags$h3(tags$span(class="fas fa-chart-bar icon"),
                   "Biomarker discovery module"),
           div(class="container-fluid shiny-code-container well",
               class = "justify-text",
               HTML(Biomarker_discovery_module)
           ),
           
           tags$h3(tags$span(class="fas fa-code icon"),
                   "Implementation in R"),
           div(class="container-fluid shiny-code-container well",
               class = "justify-text",
               HTML(Implementation_in_R)
           ),
            
           tags$h3(tags$span(class="fas fa-copyright"),
                   "Citing PhenoMultiOmics"),
           div(class="container-fluid shiny-code-container well",
               class = "justify-text",
               HTML(Citing_PhenoMultiOmics)
           )
           
    )
  )
)







