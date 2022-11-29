### EnhancerExplorer
EnhancerExplorer is a publicly available web application that provides annotation of non-coding regulatory elements obtained from genome-wide assessments of enhancers in human ESCs, NSCs, and fetal brain. 
It allows the exploration of putative functional enhancers, derived from epigenome data sets as well as functionally validated enhancers from functional genomics assays, in a user-friendly manner. It also enables users to explore various enhancer characteristics including sequence constrained, as well as predicted enhancer target genes, expression of these target genes and phenotypes associated with dysfunction of these genes. The various outputs are provided using several intuitive visualization tools. Moreover, users can upload their own query datasets of their genomic regions of interest and intersect them with the enhancer catalogue, allowing further downstream investigations.

#### Installation

##### Run EnhancerExplorer locally in R-studio
- Install R/R-studio and the required R packages (install.packages(c('shiny','ggplot2','data.table','DT','dplyr','tidyr','tibble','VennDiagram','shinyjs','htmltools','tools','gridExtra','fst','reshape2','ggpubr','egg'), type="source")).
- Download the app.R and Data files from https://figshare.com/projects/EnhancerExplorer/146736.
- Run app.R in R-studio.

##### Run EnhancerExplorer by pulling data from Docker hub
###### For Linux:
- Install Docker using https://docs.docker.com/desktop/install/linux-install/ or https://anaconda.org/conda-forge/docker.
- Run `docker run -p 8038:8038 -d soheilyousefi/enhancerexplorer:V2`.
- Navigate to `http://localhost:8038/`.
###### For Windows:
- Download and install Docker Desktop from https://www.docker.com/.
- Open the installed Docker Desktop.
- Open Command Prompt and run `docker run -p 8038:8038 -d soheilyousefi/enhancerexplorer:V2`.
- Navigate to `http://localhost:8038/`.
