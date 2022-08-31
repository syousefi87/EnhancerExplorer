# EnhancerExplorer
EnhancerExplorer is a publicly available web application that provides annotation of non-coding regulatory elements obtained from genome-wide assessments of enhancers in human ESCs, NSCs, and fetal brain. 
It allows the exploration of putative functional enhancers, derived from epigenome data sets as well as functionally validated enhancers from functional genomics assays, in a user-friendly manner. It also enables users to explore various enhancer characteristics including sequence constrained, as well as predicted enhancer target genes, expression of these target genes and phenotypes associated with dysfunction of these genes. The various outputs are provided using several intuitive visualization tools. Moreover, users can upload their own query datasets of their genomic regions of interest and intersect them with the enhancer catalogue, allowing further downstream investigations.

EnhancerExplorer can be seen on `http//:....` .

### Installation

#### Run EnhancerExplorer locally in R-studio
- Install R/R-studio and the required R packages (install.packages(c("data.table", "shiny", "DT", "dplyr"), type="source")).
- Download the app.R and Data files in the same folder.
- Run app.R in R-studio.

#### If using the local GitHub, run the following command in R or Rstudio to download and install the required packages.
- install.packages(c("data.table", "shiny", "DT", "dplyr","devtools"), type="source")
- devtools::install_github("syousefi87/EnhancerExplorer")
- EnhancerExplorer::runApp()

#### Run EnhancerExplorer using Docker
##### For Linux:
- Install Docker --> https://docs.docker.com/desktop/install/linux-install/ or https://anaconda.org/conda-forge/docker.
- Run `docker run -p 3838:3838 -d soheilyousefi/enhancerexplorer:V1`.
- Navigate to http://localhost:3838/.
##### For Windows:
- Download Docker Desktop from https://www.docker.com/.
- Install the downloaded '.exe' file.
- Open the installed Docker Desktop.
- Open Command Prompt and run `docker run -p 3838:3838 -d soheilyousefi/enhancerexplorer:V1`.
- Navigate to http://localhost:3838/.
