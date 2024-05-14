##############UI.tutorial
library(shiny)
library(shinythemes)
library(shinydashboard)

Tutorial_Multiomics_Network <- HTML(
  '<br>This section leverages data from the Multiomics DB to extract and visualize multi-omics data related to unique enzymatic metabolic reactions in various cancers. It includes two primary tabs: "Upload File" and "MultiOmics Network." The main functionalities are cancer type selection, data uploading, and network graph creation. Below is a guide to using these features.</p>',
  '<li>Choose the "Disease" 
  <br>Choose the "Disease" for analysis from the dropdown menu and click "Search". The main panel on the right will display nodes and edges files based on data extracted from the Multiomics DB for the selected cancer, with gastric cancer data being the sample data.</li>',
  '<li>File Upload 
  <br>Upload a CSV file containing a column named "name" which should include genes, proteins, or metabolites. Genes are denoted by GENE SYMBOL (e.g., CFTR), proteins by EC number (e.g., EC:2.7.10.2), and metabolites by their names (e.g., L-dopaquinone).
  <br>Upon uploading, the server automatically extracts relevant multi-omics data based on the cancer’s specific enzymatic metabolic reactions, generating nodes and edges files. These files are displayed in the main panel and can be downloaded in CSV format for further research. If there are no specific genes, proteins, or metabolites of interest, you can opt not to upload a file and instead extract the complete multi-omics data for a particular cancer.</li>',
  '<li>MultiOmics Network
  <br>Choose the data type of Sample data or Upload nodes data. If you uploaded data, select "Upload nodes data".Adjust the Range of nodes size and Layout, then click "Plot" to generate the network in the main panel.
  <br>The network is interactive; clicking on nodes displays their names. A table below the network shows node sizes which larger nodes indicate higher involvement in the cancer"s enzymatic reactions, and that can be downloaded.</li>',
  '</ul></li>',
  '</ol>'
)



Tutorial_Functional_Analysis <- HTML(
  '<br>This section focuses on gene differential analysis and KEGG enrichment analysis. Here are the steps for conducting these analyses:</p>',
  '<ol>',
  '<li><strong>DE Analysis</strong> <ul>',
  '<li>Upload Gene File 
  <br>Upload a CSV file containing gene expression data, formatted with gene names as rows and sample names as columns.</li>',
  '<li>Upload Sample Group File 
  <br>Upload a CSV file with a "group" column, labeling the experimental group as "treat" and the control group as "control". This part is limited to two-group differential expression analysis. The main panel shows the uploaded data table by default, typically gastric cancer transcriptomics data from public databases.</li>',
  '<li>Differential Expression Analysis: 
  <br>Choose a P value and input a LogFC value. Click "Calculate" to display the differential analysis results, which are downloadable. The panel also summarizes upregulated, downregulated, and non-significant findings.</li>',
  '<li>Volcano Plot
  <br>Download the DE analysis results as a CSV file, upload it here, and click "Plot" to generate a volcano plot visualizing the differential analysis outcomes.</li>',
  '</ul></li>',
  '<li><strong>GSP Analysis</strong> <ul>',
  '<li>Data Upload 
  <br>Independent of the DE analysis, upload your differential gene expression data CSV file. Choose the P value, q value, and adjustment method, then click "Calculate". The analysis results will be shown in the table below the right panel.</li>',
  '<li>GSP Analysis 
  <br>In the GSP analysis module, select the number of enrichment pathways to display and click "Calculate". The results are presented as bar and bubble charts.</li>',
  '<li>KEGG Network 
  <br>Input the desired number of pathways, choose a layout, and click "Calculate". The right panel will then display a network diagram showing specific genes enriched in different pathways.</li>',
  '</ul></li>',
  '</ol>'
)

tabPanel(
  "Tutorial",
  fluidRow(
    column(12,
           
           tags$h3(tags$span(class="fas fa-network-wired icon"), "MultiOmics Network"),
           div(class="container-fluid shiny-code-container well", HTML(Tutorial_Multiomics_Network)),
           
           tags$h3(tags$span(class="fas fa-chart-bar icon"), "Functional Analysis"),
           div(class="container-fluid shiny-code-container well", HTML(Tutorial_Functional_Analysis))
           
           
    ))
)

