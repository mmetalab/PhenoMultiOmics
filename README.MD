# PhenoMultiOmics

## Overview

**PhenoMultiOmics** is an enzymatic reaction-inferred multi-omics network visualization web server designed to explore and visualize multi-omics networks across various cancer types. The platform integrates gene, protein, and metabolite data associated with enzymatic reactions to facilitate comprehensive insights into disease-related perturbations at the molecular level.

The web server offers functionalities such as differential expression analysis, gene set pathway enrichment analysis, and visualization of multi-omics networks. It is particularly useful for studying disrupted enzymatic reactions and their impacts on cancer biology.

## Features

- **Multi-Omics Database (PMODB):** Stores data records of enzymatic reactions involving genes, proteins, metabolites, and phenotypes. Users can query and download the relationships between these biological entities.
- **Biomarker Discovery Module:** Allows for differential expression analysis, functional analysis, and multi-omics network visualization. Supports univariate and multivariate analysis and provides tools for pathway enrichment analysis.
- **Multi-Omics Network Visualization:** Displays the interplay between genes, proteins, and metabolites based on enzymatic reactions, highlighting disruptions related to diseases.
- **Web Interface:** User-friendly, interactive interface accessible at [PhenoMultiOmics](https://phenomultiomics.shinyapps.io/cancer/).

## Installation

### Requirements

- **R (version 4.4.1)**
- R packages:
  - `visNetwork`
  - `limma`
  - `ggplot2`
  - `clusterProfiler`
  - Additional packages as needed

### Installation Steps

1. Clone the repository:

   ```
   git clone https://github.com/mmetalab/PhenoMultiOmics.git
   ```

2. Navigate to the repository directory:

   ```
   cd PhenoMultiOmics
   ```

3. Install the required R packages:

   ```
   install.packages(c("visNetwork", "limma", "ggplot2", "clusterProfiler"))
   ```

4. Run the web server locally:

   ```
   shiny::runApp("app")
   ```

## Usage

1. **Data Upload:** Users can upload their preprocessed multi-omics datasets, including transcriptomics, proteomics, and metabolomics data.
2. **Analysis:**
   - Perform differential expression analysis using the biomarker discovery module.
   - Conduct pathway enrichment analysis and visualize results.
   - Generate and customize multi-omics networks based on the enzymatic reaction data.
3. **Visualization:** Interactively explore the generated multi-omics networks and export the results for further analysis.

## Example Datasets

The repository includes example datasets for gastric cancer transcriptomics and lung cancer metabolomics, which can be used to test the functionalities of the PhenoMultiOmics platform.

## Data Sources

PhenoMultiOmics integrates data from multiple sources, including:

- Metabolic Atlas
- BRENDA database
- RHEA database
- EnzymeMap database
- DisGeNET
- UniProt Knowledgebase

## Contributions

We welcome contributions to enhance the PhenoMultiOmics platform. Please submit pull requests or report issues on the [GitHub repository](https://github.com/mmetalab/PhenoMultiOmics).

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgements

This project was supported by the Shandong Natural Science Foundation and the National Natural Science Foundation of China. We also acknowledge the computing resources provided by the National Institute of Health Data Science of China.