#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#http://shiny.rstudio.com/

library(shiny)
library(data.table)
library(DT)
library(dplyr)
library(VennDiagram)
library(ggplot2)
library(gridExtra)
library(fst)

options(rsconnect.max.bundle.files = 3145728000)
####################
#################### ESC data Input
####################

ESC.enhancer_Vista <- read.fst( "ESC_Enhancer/ESC_Vista.table.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
ESC_Gene <- read.fst( "ESC_Enhancer/ESC_hg19Gene.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
ESC_OMIM <- read.fst( "ESC_Enhancer/ESC_phenotype.hg19.OMIM.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
ESC_Clinvar <- read.fst( "ESC_Enhancer/ESC_phenotype.hg19.ClinVar.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
ESC_GWAS <- read.fst( "ESC_Enhancer/ESC_phenotype.hg19.GWAS.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
ESC_DNA.features.table <- read.fst( "ESC_Enhancer/ESC_GC.phastCons.nCER.LoF.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
ESC.Expression.melt <- read.fst( "ESC_Enhancer/ESC_Expression.RPKM_hg19.melt.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)

####################
#################### NSC data Input
####################

NSC.enhancer_Vista <- read.fst( "NSC_Enhancer/NSC_Vista.table.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
NSC_Gene <- read.fst( "NSC_Enhancer/NSC_hg19Gene.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
NSC_OMIM <- read.fst( "NSC_Enhancer/NSC_phenotype.hg19.OMIM.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
NSC_Clinvar <- read.fst( "NSC_Enhancer/NSC_phenotype.hg19.ClinVar.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
NSC_GWAS <- read.fst( "NSC_Enhancer/NSC_phenotype.hg19.GWAS.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
NSC_DNA.features.table <- read.fst( "NSC_Enhancer/NSC_GC.phastCons.nCER.LoF.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
NSC.Expression.melt <- read.fst( "NSC_Enhancer/Expression.RPKM_cells.melt.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)

####################
#################### Fetal Brain data Input
####################

FB.enhancer_Vista <- read.fst( "Fetal_Brain/FB_Vista.table.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
FB_Gene <- read.fst( "Fetal_Brain/FB_hg19.Gene.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
FB_OMIM <- read.fst( "Fetal_Brain/FB_phenotype.hg19.OMIM.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
FB_Clinvar <- read.fst( "Fetal_Brain/FB_phenotype.hg19.ClinVar.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
FB_GWAS <- read.fst( "Fetal_Brain/FB_phenotype.hg19.GWAS.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
FB_DNA.features.table <- read.fst( "Fetal_Brain/FB_GC.phastCons.nCER.LoF.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)
FB.Expression.melt <- read.fst( "Fetal_Brain/Expression_melt.bed",from = 1,to = NULL,as.data.table = T,old_format = FALSE)

########
########  Shinny App
########

################################################     ########################
################################################ UI  ########################
################################################     ########################
ui <- fluidPage(tags$head (tags$style (
  HTML ('<hr style="border-color: #0088cc;">'))),
  titlePanel (title = span ("EnhancerExplorer")),
  h4 ("A web application to explore putative functional enhancers", align = "justify", style = "color:grey"),
  tags$br (),
  shinyjs::useShinyjs(), # needed for download button to work
  tabsetPanel( #type = "pills",
    #######
    ####### About                   
    #######
    tabPanel("About", icon = icon("book"), align = "justify",
             br(),
p("~98% of the human genome does not directly encode protein coding genes, but contains important regulatory sequences such as enhancers  that ensure correct spatiotemporal gene expression. 
Alterations to enhancers can cause human disease, but finding enhancers in the genome by non-bioinformaticians is often complicated. 
Here we present EnhancerExplorer, a user-friendly web-application that allows to browse (putative) functional enhancers from human embryonic stem cells, neural stem cells and fetal brain. 
Amongst other information, EnhancerExplorer allows to explore enhancer-gene interactions, information on linked diseases and sequence characteristics of enhancers, which will help to interpret non-coding variation identified in patients.")
),
    #######
    ####### ESC panel                   
    #######
    tabPanel("ESC Enhancers", icon = icon("database"),
             tabsetPanel(type = "pills",
                         tabPanel("Enhancer Table",icon = icon("table"),
                                  sidebarLayout(
                                    sidebarPanel (
                                      p(strong ("Filter enhancer data"),style = "color:blue;"),
                                      radioButtons ("ESC.table.celltype", "Choose cell type:", c ("Primed (hESC-H9)" = "Primed", "Naive (hESC-H9)" = "Naive")),
                                      radioButtons ("ESC.table.allrank","Choose activity type:", c("All enhancer activity" = "All enhancer activity", "Filter enhancer activity" = "Filter enhancer activity")),
                                      sliderInput("ESC.table.rank", label =div(style='width:175px;',div(style='float:none;', ''),
                                                                               div(style='float:left;', 'The least',style ="font-weight: normal;"),
                                                                               div(style='float:right;', 'The most',style ="font-weight: normal;")),min = 1, max = 10, value = c(8,10),step=1),
                                      tags$hr(), 
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("ESCTableQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                      conditionalPanel("input.ESCTableQueryChoose=='file'",
                                                       fileInput("ESC.Table.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.ESCTableQueryChoose=='text'",
                                                       textAreaInput("ESC.Table.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("ESC.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("ESC.download", "Download results", style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("ESC_table"),
                                      dataTableOutput("ESC_overlap_table"),
                                      fluidRow(column(5,plotOutput("ESC_overlap_plot"))))
                                  )
                         ),
                         tabPanel("Enhancer Gene",icon = icon("table"),
                                  sidebarLayout (
                                    sidebarPanel (
                                      p(strong ("Filter enhancer data"),style = "color:blue;"),
                                      radioButtons ("ESC.Gene.celltype", "Choose cell type:", c ("Primed (hESC-H9)" = "Primed", "Naive (hESC-H9)" = "Naive")),
                                      radioButtons ("ESC.Gene.allrank","Choose activity type:", c("All enhancer activity" = "All enhancer activity", "Filter enhancer activity" = "Filter enhancer activity")),
                                      sliderInput("ESC.Gene.rank", label =div(style='width:175px;',div(style='float:none;', ''),
                                                                              div(style='float:left;', 'The least',style ="font-weight: normal;"),
                                                                              div(style='float:right;', 'The most',style ="font-weight: normal;")),min = 1, max = 10, value = c(8,10),step=1),
                                      tags$hr(), 
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                       selectInput("ESCGeneQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                      conditionalPanel("input.ESCGeneQueryChoose=='file'",
                                       fileInput("ESC.Gene.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.ESCGeneQueryChoose=='text'",
                                                       textAreaInput("ESC.Gene.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder = "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("ESC.Gene.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("ESC.Gene.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("ESC.Gene_table"),
                                              dataTableOutput("ESC.Gene_overlap_table"))
                                  )
                         ), 
                         tabPanel("Enhancer Phenotype",icon = icon("table"),
                                  sidebarLayout (
                                    sidebarPanel (
                                      radioButtons ("ESC.Phenotype.Choose.source", "Choose phenotype source:", c ("OMIM" = "OMIM", "ClinVar" = "ClinVar","GWAS" = "GWAS")),
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("ESCPhenotypeQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                      conditionalPanel("input.ESCPhenotypeQueryChoose=='file'",
                                      fileInput("ESC.Phenotype.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.ESCPhenotypeQueryChoose=='text'",
                                                       textAreaInput("ESC.Phenotype.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("ESC.Phenotype.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("ESC.Phenotype.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("ESC.Phenotype_table"),
                                              dataTableOutput("ESC.Phenotype_overlap_table"))
                                  )
                         ),
                         tabPanel ("Visualization", icon = icon ("image"),
                                   tabsetPanel (type = "pills", 
                                                tabPanel("Expression",icon = icon("object-group"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             textAreaInput('ESC.Expression.Gene', 'Enter gene name (hg19):', value = "", width = "100%" ,placeholder ="Separate by Enter"),
                                                             radioButtons ("ESC.Expression.Choose.plot", "Choose plot type:", 
                                                                           c ("Line plot" = "Line plot", "Bar plot" = "Bar plot","Heatmap plot" = "Heatmap plot")),
                                                             div(actionButton("ESC.Expression.plot", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black")),
                                                             br(),br(),
                                                             downloadButton("ESC.Expression.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("ESC.Expression_plot"),
                                                                     DTOutput("ESC.Expression_table"))
                                                         )
                                                ),
                                                tabPanel("pLI",icon = icon("chart-bar"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             textAreaInput('ESC.pLI.Gene', 'Enter gene name (hg19):', value = "", width = "100%" ,placeholder ="Separate by Enter"),
                                                             div(actionButton("ESC.pLI.plot", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black")),
                                                             br(),br(),
                                                             downloadButton("ESC.pLI.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("ESC.pLI_plot"))
                                                         )
                                                ),
                                                tabPanel("DNA Features",icon = icon("chart-bar"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                                             br(),
                                                             selectInput("ESCDNAfeaturesQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                                             conditionalPanel("input.ESCDNAfeaturesQueryChoose=='file'",
                                                             fileInput("ESC.DNA.features.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                                             conditionalPanel("input.ESCDNAfeaturesQueryChoose=='text'",
                                                             textAreaInput("ESC.DNA.features.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder = "Like chr1,100,200 per line with chr,start,end as header")),
                                                             actionButton("ESC.DNA.features.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                                             br(),br(),
                                                             downloadButton("ESC.DNA.features.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("ESC.DNA.features_plot"),
                                                                     DTOutput("ESC.DNA.features_table"))
                                                           )
                                                         )
                                                )
                                   ),
                         tabPanel("Genome Browser",icon = icon("image"),
                                  h3("Enhancer visualization across epigenome data"), 
                                  p(""),
                                  p("In this",a( href= "https://genome.ucsc.edu/s/soheil01/EnhancerExplorer_ESC.Tab_hg19",target="_blank",strong("UCSC browser session")),"you can easily check the location of enhancers which are linked to their target genes (including all interactions in `Enhancer Gene` tab) across different epigenome data."),
                                  p("Epigenome data include STARR-seq data of H3K27ac, H3K4me1, NANOG and OCT4, ChIP-seq data of H3K27ac,H3K4me1,NANOG and OCT4, and RNA-seq data."),
                                  strong("This genome browser shows:"),
                                  tags$ul(
                                    tags$li("1: Entering your gene or genomic region of interest."),
                                    tags$li("2: Enhancer and its target gene (category: enhancer activity category; Primed: Primed hESC_H9; Naive: Naive hESC_H9)."),
                                    tags$li("3: Different epigenome data tracks are shown in different colors.")
                                  ),
                                  br(),
                                  imageOutput("ESC_UCSC_sample")
                         ),
                         tabPanel("Data Summary", icon = icon("pen"), align = "justify",
                                  h3("Summary"),
                                  tags$ul(
                                    tags$li("The genome-wide functional enhancer landscape in Primed and Naive hESCs was investigated, using a massively parallel reporter assay, called ChIP-STARR-seq."),
                                    tags$li("Massively parallel reporter assay assessed over 350 thousand genomic regions bound by NANOG or OCT4, or marked by H3K27ac or H3K4me1."),
                                    tags$li("Enhancers were ranked into 10 categories based on enhancer acticity where category 1 refers to enhancers with the least activity and category 10 refers to enhancers with the most activity."),
                                    tags$li("Enhancers were linked to their target genes using TSS, Hi-C, JEME and GeneHancer."),
                                    tags$li("Enhancers and their target genes were linked to disease phenotypes.")
                                    ),
                                  h3("Further Information"),
                                  p(a(href="https://www.cell.com/cell-stem-cell/pdfExtended/S1934-5909(18)30296-0",target="_blank","Barakat TS, et al. Functional Dissection of the Enhancer Repertoire in Human Embryonic Stem Cells. Cell Stemm Cell 23(2)(2018). https://doi.org/10.1016/j.stem.2018.06.014"))
                                  )
                         )
             ),
    #######
    ####### NSC panel
    #######
    tabPanel("NSC Enhancers", icon = icon("database"),
             tabsetPanel(type = "pills",
                         tabPanel("Enhancer Table",icon = icon("table"),
                                  sidebarLayout(
                                    sidebarPanel (
                                      p(strong ("Filter enhancer data"),style = "color:blue;"),
                                      radioButtons ("NSC.table.celltype", "Choose cell type:", c ("NSC (NSC ChIP data in NSC)" = "NSC", "ESC (NSC ChIP data in ESC)" = "ESC")),
                                      radioButtons ("NSC.table.allrank","Choose activity type:", c("All enhancer activity" = "All enhancer activity", "Filter enhancer activity" = "Filter enhancer activity")),
                                      sliderInput("NSC.table.rank", label =div(style='width:175px;',div(style='float:none;', ''),
                                                                               div(style='float:left;', 'The least',style ="font-weight: normal;"),
                                                                               div(style='float:right;', 'The most',style ="font-weight: normal;")),min = 1, max = 10, value = c(8,10),step=1),
                                      tags$hr(), 
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("NSCTableQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                      conditionalPanel("input.NSCTableQueryChoose=='file'",
                                      fileInput("NSC.Table.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.NSCTableQueryChoose=='text'",
                                      textAreaInput("NSC.Table.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder = "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("NSC.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("NSC.download", "Download results", style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(
                                      dataTableOutput("NSC_table"),
                                      dataTableOutput("NSC_overlap_table"),
                                      fluidRow(column(5,plotOutput("NSC_overlap_plot")))
                                      )
                                    )
                                  ),
                         tabPanel("Enhancer Gene",icon = icon("table"),
                                  sidebarLayout (
                                    sidebarPanel (
                                      p(strong ("Filter enhancer data"),style = "color:blue;"),
                                      radioButtons ("NSC.Gene.celltype", "Choose cell type:", c ("NSC (NSC ChIP-STARR-seq in NSC)" = "NSC", "ESC (NSC ChIP-STARR-seq data in ESC)" = "ESC")),
                                      radioButtons ("NSC.Gene.allrank","Choose activity type:", c("All enhancer activity" = "All enhancer activity", "Filter enhancer activity" = "Filter enhancer activity")),
                                      sliderInput("NSC.Gene.rank", label =div(style='width:175px;',div(style='float:none;', ''),
                                                                              div(style='float:left;', 'The least',style ="font-weight: normal;"),
                                                                              div(style='float:right;', 'The most',style ="font-weight: normal;")),min = 1, max = 10, value = c(8,10),step=1),
                                      tags$hr(),  
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("NSCGeneQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                      conditionalPanel("input.NSCGeneQueryChoose=='file'",
                                      fileInput("NSC.Gene.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.NSCGeneQueryChoose=='text'",
                                      textAreaInput("NSC.Gene.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("NSC.Gene.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("NSC.Gene.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("NSC.Gene_table"),
                                              dataTableOutput("NSC.Gene_overlap_table"))
                                    )
                                  ), 
                         tabPanel("Enhancer Phenotype",icon = icon("table"),
                                  sidebarLayout (
                                    sidebarPanel (
                                      radioButtons ("NSC.Phenotype.Choose.source", "Choose phenotype source:", c ("OMIM" = "OMIM", "ClinVar" = "ClinVar","GWAS" = "GWAS")),
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("NSCPhenotypeQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                      conditionalPanel("input.NSCPhenotypeQueryChoose=='file'",
                                      fileInput("NSC.Phenotype.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.NSCPhenotypeQueryChoose=='text'",
                                      textAreaInput("NSC.Phenotype.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("NSC.Phenotype.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("NSC.Phenotype.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("NSC.Phenotype_table"),
                                              dataTableOutput("NSC.Phenotype_overlap_table"))
                                    )
                                  ),
                         tabPanel ("Visualization", icon = icon ("image"),
                                   tabsetPanel (type = "pills", 
                                                tabPanel("Expression",icon = icon("object-group"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             textAreaInput('NSC.Expression.Gene', 'Enter gene name (hg19):', value = "", width = "100%" ,placeholder ='Separate by Enter'),
                                                             radioButtons ("NSC.Expression.Choose.plot", "Choose plot type:", 
                                                                           c ("Line plot" = "Line plot", "Bar plot" = "Bar plot","Heatmap plot" = "Heatmap plot")),
                                                             div(actionButton("NSC.Expression.plot", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black")),
                                                             br(),br(),
                                                             downloadButton("NSC.Expression.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("NSC.Expression_plot"),
                                                                     DTOutput("NSC.Expression_table"))
                                                           )
                                                         ),
                                                tabPanel("pLI",icon = icon("chart-bar"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             textAreaInput('NSC.pLI.Gene', 'Enter gene name (hg19):', value = "", width = "100%" ,placeholder ='Separate by Enter'),
                                                             div(actionButton("NSC.pLI.plot", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black")),
                                                             br(),br(),
                                                             downloadButton("NSC.pLI.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("NSC.pLI_plot"))
                                                           )
                                                         ),
                                                tabPanel("DNA Features",icon = icon("chart-bar"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                                             br(),
                                                             selectInput("NSCDNAfeaturesQueryChoose","Choose file source",choices = c("file","text"),selected = "file"),
                                                             conditionalPanel("input.NSCDNAfeaturesQueryChoose=='file'",
                                                             fileInput("NSC.DNA.features.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                                             conditionalPanel("input.NSCDNAfeaturesQueryChoose=='text'",
                                                             textAreaInput("NSC.DNA.features.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder = "Like chr1,100,200 per line with chr,start,end as header")),
                                                             actionButton("NSC.DNA.features.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                                             br(),br(),
                                                             downloadButton("NSC.DNA.features.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("NSC.DNA.features_plot"),
                                                                     DTOutput("NSC.DNA.features_table"))
                                                           )
                                                         )
                                                )
                                   ),
                         tabPanel("Genome Browser",icon = icon("image"),
                                  h3("Enhancer visualization across epigenome data"), 
                                  p(""),
                                  p("In this",a( href= "https://genome.ucsc.edu/s/soheil01/EnhancerExplorer_NSC.Tab_hg19",target="_blank",strong("UCSC browser session")),"you can easily check the location of enhancers which are linked to their target genes (including all interactions in `Enhancer Gene` tab) across different epigenome data."),
                                  p("Epigenome data including STARR-seq data of transcription factors YY1 and SOX2, and histone modifications of H3K27ac and H3K4me1, ATAC-seq data, ChIP-seq data of H3K27ac, H3K4me1, YY1 and SOX2, and RNA-seq data."),
                                  strong("This genome browser shows:"),
                                  tags$ul(
                                    tags$li("1: Entering your gene or genomic region of interest."),
                                    tags$li("2: Enhancer and its target gene (Category: enhancer activity category; NSC: NSC ChIP data differentiated in NSCs; ESC: NSC ChIP data differentiated in ESCs)."),
                                    tags$li("3: Different epigenome data tracks are shown in different colors.")
                                  ),
                                  br(),
                                  imageOutput("NSC_UCSC_sample")
                         ),
                         tabPanel("Data Summary", icon = icon("pen"), align = "justify",
                                  h3("Summary"),
                                  tags$ul(
                                    tags$li("The genome-wide functional enhancer landscape in NSCs was investigated, using a massively parallel reporter assay, called ChIP-STARR-seq."),
                                    tags$li("Massively parallel reporter assay assessed over 148 thousand genomic regions bound by SOX2 or YY1, or marked by H3K27ac or H3K4me1."),
                                    tags$li("Enhancers were ranked into 10 categories based on enhancer acticity where category 1 refers to enhancers with the least activity and category 10 refers to enhancers with the most activity."),
                                    tags$li("The same NSC-derived plasmid libraries were investigated in ESCs  to define differential enhancer activity between NSCs and ESCs."),
                                    tags$li("Enhancers were linked to their target genes using TSS, Hi-C, JEME and GeneHancer."),
                                    tags$li("Enhancers and their target genes were linked to disease phenotypes.")
                                    ),
                                  h3("Further Information"),
                                  p("Perenthaler E, et al. Identification of the active enhancer landscape in Neural Stem Cells by ChIP-STARR-seq. In prepration")
                                  )
                         )
             ),
    #######
    ####### Fetal Brain panel
    #######
    tabPanel("Fetal Brain Enhancers", icon = icon("database"),
             tabsetPanel(type = "pills",
                         tabPanel("Enhancer Table",icon = icon("table"),
                                  sidebarLayout(
                                    sidebarPanel (
                                      radioButtons ("FB.Enhancer.type", "Choose enhancer type:", c ("DEA" = "DAE", "nDAE" = "nDAE", "All"="All"),selected="DAE"),
                                      tags$hr(),
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("FBTableQueryChoose","Choose file source",choices = c("file","text"),selected = NULL),
                                      conditionalPanel("input.FBTableQueryChoose=='file'",
                                      fileInput("FB.Table.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.FBTableQueryChoose=='text'",
                                      textAreaInput("FB.Table.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("FB.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("FB.download", "Download results", style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("FB_table"),
                                              dataTableOutput("FB_overlap_table"),
                                              fluidRow(column(5,plotOutput("FB_overlap_plot"))))
                                    )
                                  ),
                         tabPanel("Enhancer Gene",icon = icon("table"),
                                  sidebarLayout (
                                    sidebarPanel (
                                      radioButtons ("FB.Gene.type", "Choose enhancer type:", c ("DEA" = "DAE", "nDAE" = "nDAE", "All"="All"),selected="DAE"),
                                      tags$hr(), 
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("FBGeneQueryChoose","Choose file source",choices = c("file","text"),selected = NULL),
                                      conditionalPanel("input.FBGeneQueryChoose=='file'",
                                      fileInput("FB.Gene.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.FBGeneQueryChoose=='text'",
                                      textAreaInput("FB.Gene.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("FB.Gene.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("FB.Gene.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("FB.Gene_table"),
                                              dataTableOutput("FB.Gene_overlap_table"))
                                    )
                                  ), 
                         tabPanel("Enhancer Phenotype",icon = icon("table"),
                                  sidebarLayout (
                                    sidebarPanel (
                                      radioButtons ("FB.Phenotype.Choose.source", "Choose phenotype source:", c ("OMIM" = "OMIM", "ClinVar" = "ClinVar","GWAS" = "GWAS")),
                                      p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                      br(),
                                      selectInput("FBPhenotypeQueryChoose","Choose file source",choices = c("file","text"),selected = NULL),
                                      conditionalPanel("input.FBPhenotypeQueryChoose=='file'",
                                      fileInput("FB.Phenotype.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                      conditionalPanel("input.FBPhenotypeQueryChoose=='text'",
                                      textAreaInput("FB.Phenotype.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                      actionButton("FB.Phenotype.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                      br(),br(),
                                      downloadButton("FB.Phenotype.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                      width = "2"),
                                    mainPanel(dataTableOutput("FB.Phenotype_table"),
                                              dataTableOutput("FB.Phenotype_overlap_table"))
                                    )
                                  ), 
                         tabPanel ("Visualization", icon = icon ("image"),
                                   tabsetPanel (type = "pills", 
                                                tabPanel("Expression",icon = icon("object-group"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             textAreaInput('FB.Expression.Gene', 'Enter gene name (hg19):', value = "", width = NULL ,placeholder ='Separate by Enter'),
                                                             radioButtons ("FB.Expression.Choose.plot", "Choose plot type:", 
                                                                           c ("Line plot" = "Line plot", "Bar plot" = "Bar plot","Heatmap plot" = "Heatmap plot")),
                                                             div(actionButton("FB.Expression.plot", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black")),
                                                             br(),br(),
                                                             downloadButton("FB.Expression.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("FB.Expression_plot"),
                                                                     DTOutput("FB.Expression_table"))
                                                           )
                                                         ),
                                                tabPanel("pLI",icon = icon("chart-bar"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             textAreaInput('FB.pLI.Gene', 'Enter gene name (hg19):', value = "", width = "100%" ,placeholder ='Separate by Enter'),
                                                             div(actionButton("FB.pLI.plot", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black")),
                                                             br(),br(),
                                                             downloadButton("FB.pLI.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("FB.pLI_plot"))
                                                           )
                                                         ),
                                                tabPanel("DNA Features",icon = icon("chart-bar"),
                                                         sidebarLayout(
                                                           sidebarPanel(
                                                             p(strong ("Find overlap between your file and enhancer data"),style = "color:blue;"),
                                                             br(),
                                                             selectInput("FBDNAfeaturesQueryChoose","Choose file source",choices = c("file","text"),selected = NULL),
                                                             conditionalPanel("input.FBDNAfeaturesQueryChoose=='file'",
                                                             fileInput("FB.DNA.features.query.file", "Upload genomic coordinates in .bed format:",multiple = F,accept = ".bed")),
                                                             conditionalPanel("input.FBDNAfeaturesQueryChoose=='text'",
                                                             textAreaInput("FB.DNA.features.query.text", "Enter comma separated genomic coordinates with chr,start,end as header:",placeholder =  "Like chr1,100,200 per line with chr,start,end as header")),
                                                             actionButton("FB.DNA.features.run", label="Run overlap",icon("paper-plane"), style="color: black; background-color: #06F9E1; border-color: black"),
                                                             br(),br(),
                                                             downloadButton("FB.DNA.features.download", "Download results",style="color: white; background-color: gray; border-color: black"),
                                                             width = "2"),
                                                           mainPanel(plotOutput("FB.DNA.features_plot"),
                                                                     DTOutput("FB.DNA.features_table"))
                                                           )
                                                         )
                                                )
                                   ),
                         tabPanel("Genome Browser",icon = icon("image"),
                                  h3("Enhancer visualization across epigenome data"), 
                                  p(""),
                                  p("In this",a( href= "https://genome.ucsc.edu/s/soheil01/EnhancerExplorer_Fetal.Brain.Tab_hg19",target="_blank",strong("UCSC browser session")),"you can easily check the location of enhancers which are linked to their target genes (including all interactions in `Enhancer Gene` tab) across different epigenome data."),
                                  p("Epigenome data are collected from the Roadmap Epigenomics Consortium, ENCODE, PsychENCODE, and other studies. Epigenome data are from different brain regions and various developmental stages."),
                                  strong("This genome browser shows:"),
                                  tags$ul(
                                    tags$li("1: Entering your gene or genomic region of interest"),
                                    tags$li("2: DAE and its target gene"),
                                    tags$li("3: nDAE and its target gene"),
                                    tags$li("4: Different epigenome data tracks from different developmental stages and anatomical parts of the fetal brain. They are shown in different colors.")
                                  ),
                                  br(),
                                  imageOutput("FB_UCSC_sample")
                         ),
                         tabPanel("Data Summary", icon = icon("pen"), align = "justify",
                                  h3("Summary"),
                                  tags$ul(
                                    tags$li("~1.6 million putative brain enhancers were collected from all available databases."),
                                    tags$li("The maximum overlap between putative brain enhancers were called putative critical regions (pCRs)."),
                                    tags$li("~500 fetal brain related epigenome data sets re-analyzed."),
                                    tags$li("Putative critical regions with high variability acrosss epigenome data were called differentially active enhancers (DAEs) and regions with more constant epigenome pattern were defined as non-differentially active enhancers (nDAEs)."),
                                    tags$li("Enhancers were linked to their target genes using several approaches."),
                                    tags$li("Enhancers and their target genes were linked to diseases.")
                                    ),
                                  h3("Further Information"),
                                  p(a(href="https://genomemedicine.biomedcentral.com/articles/10.1186/s13073-021-00980-1",target="_blank","Yousefi S, et al. Comprehensive multi-omics integration identifies differentially active enhancers during human brain development with clinical relevance. Genome Medicine 13, 162 (2021). https://doi.org/10.1186/s13073-021-00980-1"))
                                  )
                         )
             ),
    #######
    ####### Contact Us
    #######
    tabPanel("Contact Us",icon = icon("book"),
             h3("For questions and comments:"),
             p("Tahsin Stefan Barakat:" , em("t.barakat@erasmusmc.nl")) ,
             p("Soheil Yousefi:", em("s.yousefi@erasmusmc.nl")) , 
             p(a( href= "https://www.erasmusmc.nl/en/research/groups/barakat-lab-non-coding-genome-in-clinical-genetics", target="_blank", "Stefan Barakat Lab")),
             p("Department of Clinical Genetics"),
             p("Erasmus University Medical Center Rotterdam")
             )
), 
style='width: device-width, initial-scale=1; height: 1200px'
)


########################
######################## server  
########################

server <- function (input, output, session) {
  
  ####################
  #################### ESC_panel
  ####################
  
  ### ESC Enhancer Table sub-panel
  ## Select/ filter table based on cell type
  ESC_table <- reactive({
    if (input$ESC.table.allrank == "All enhancer activity") { 
      ESC_table <-  ESC.enhancer_Vista %>% unique() %>% dplyr::filter(CellType %in% input$ESC.table.celltype) %>% unique() 
    } else ESC_table <- dplyr::filter(ESC.enhancer_Vista, between(`Activity Rank`, input$ESC.table.rank[1], input$ESC.table.rank[2]), CellType %in%  input$ESC.table.celltype)%>% unique() 
    return(ESC_table)
  })
  
  output$ESC_table<- renderDataTable({
    datatable(ESC_table (), escape = FALSE, options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = FALSE),
              caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                  #htmltools::tags$span (strong ('Activity Score:'), "Enhancer Activity Score based on STARR-seq data (RNA/DNA ratio)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Activity Rank:'), "Enhancer activity level: the most active (Rank 10) and the least active (Rank 1)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Cell type:'), "Naive/Primed human embryonic stem cells (H9)", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('Enhancer activity (Vista):'), "Overlap between enhancer table and positive validated Vista enhancers", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('Expression pattern (Vista):'), "Expression pattern of validated Vista enhancers", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('VistaID:'), "Vista enhancer ID", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('VistaID.link:'), "Check enhancer in Vista Enhancer browser", style = "color:black;"),
                                                  style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  ##  Adjust to upload file up to 75MB (Default is 5 MB)
  options(shiny.maxRequestSize=75*1024^2)
  
  ## query file/text
  user_query.ESC <- reactive({
    if (input$ESCTableQueryChoose == 'file') { 
      req(input$ESC.Table.query.file)
      ext <- tools::file_ext(input$ESC.Table.query.file$name)
      switch(ext,
             bed = fread(input$ESC.Table.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$ESC.Table.query.text)) %>% setkey(chr, start, end)
  })
  
  ## overlap query file/text with enhancer data
  ESC.overlap <- eventReactive(input$ESC.run, {
    req(input$ESC.run)
      ESC.query.overlap<- foverlaps(user_query.ESC(), ESC_table () , nomatch = 0) %>% 
        unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
      ESC.query.overlap <- ESC.query.overlap %>% 
        mutate(Overlap.start = ESC.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
        mutate(Overlap.end = ESC.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
        mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 

  })
  
  ## make variable for overlap table and plot
  v_ESC.Enhancer <- reactiveValues(plot=NULL,table=NULL)
  
  observeEvent(input$ESC.run, {
    req(input$ESC.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      ESC.overlap<-ESC.overlap()%>% unique()
    })
    ## draw overlap plot
    v_ESC.Enhancer$plot <-VennDiagram::draw.pairwise.venn(
      area1 = nrow(unique(user_query.ESC()[,1:3])),
      area2 = nrow(unique(ESC_table()[,1:3])),
      cross.area = nrow(unique(ESC.overlap() [,c(1,7:8)])),
      category = c("Query", "Enhancer"), 
      col = c("black", "darkgray"),
      cex = 1.5,
      cat.cex = 2,
      euler.d =T,
      scaled=F,
      ext.text=T,
      sep.dist = 0.03,
      cat.pos = c(180, 180))
    
    v_ESC.Enhancer$table <- req(DT::datatable(ESC.overlap(),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
    
  },ignoreInit = TRUE)
  
  
  ## output
  output$ESC_overlap_table <- renderDT({ v_ESC.Enhancer$table }) 
  output$ESC_overlap_plot  <- renderPlot({ gridExtra::grid.arrange(gTree(children=v_ESC.Enhancer$plot)) })
  
  
  ## Download results
  shinyjs::disable("ESC.download")
  observeEvent(ESC.overlap(), {
    shinyjs::enable("ESC.download")
  })
  
  output$ESC.download <- downloadHandler(
    filename = "ESC.Table.download.csv",
    content = function(file) {
      write.csv(ESC.overlap(), file, row.names = FALSE,sep="\t")
    }
  )
  
  ####
  #### Enhancer Gene table sub-panel
  ####

  ## Enhancer gene table
  ## Select/filter table baed on cell type
  ESCGenetable <- reactive({
    if (input$ESC.Gene.allrank == "All enhancer activity") { 
      ESCGenetable <-  ESC_Gene %>% unique() %>% dplyr::filter(CellType %in% input$ESC.Gene.celltype) %>% unique() 
    } else ESCGenetable <- dplyr::filter(ESC_Gene, between(`Activity Rank`, input$ESC.Gene.rank[1], input$ESC.Gene.rank[2]), CellType %in%  input$ESC.Gene.celltype)%>% unique() 
    return(ESCGenetable)
  })
  
  
  output$ESC.Gene_table<- renderDataTable({
    datatable(ESCGenetable (), options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
              caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr, start, end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                  #htmltools::tags$span (strong ('Activity Score:'), "Enhancer Activity Score based on STARR-seq data (RNA/DNA ratio)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Activity Rank:'), "Enhancer activity level: the most active (Rank 10) and the least active (Rank 1)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('GeneID and GeneName:'), "Target gene (hg19)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Cell type:'), "Naive/Primed human embryonic stem cells (H9)", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('GeneDefine:'), "TSS (defined from -1 kb to +100 bp), HiC (based on 25 Kb resolution bins), JEME and GeneHancer", style = "color:black;"),
                                                  style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  ## query file/text
  user_query.ESC.Gene <- reactive({
    if (input$ESCGeneQueryChoose == 'file') { 
      req(input$ESC.Gene.query.file)
      ext <- tools::file_ext(input$ESC.Gene.query.file$name)
      switch(ext,
             bed = fread(input$ESC.Gene.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$ESC.Gene.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with enhancer data
  ESC.Gene.overlap <- eventReactive(input$ESC.Gene.run, {
    req(input$ESC.Gene.run)
      ESC.Gene.query.overlap<- foverlaps(user_query.ESC.Gene(), ESCGenetable () , nomatch = 0) %>% 
        unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
      ESC.Gene.query.overlap <- ESC.Gene.query.overlap %>% 
        mutate(Overlap.start = ESC.Gene.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
        mutate(Overlap.end = ESC.Gene.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
        mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
  })
  
  ## make variable for overlap table
  v_ESC.Gene <- reactiveValues(table=NULL)
  
  observeEvent(input$ESC.Gene.run, {
    req(input$ESC.Gene.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      ESC.Gene.overlap<-ESC.Gene.overlap()%>% unique()
    })
    
   v_ESC.Gene$table <- req(DT::datatable(ESC.Gene.overlap(),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
  },ignoreInit = TRUE)
  
  ## output
  output$ESC.Gene_overlap_table <- renderDT({ v_ESC.Gene$table }) 
  
  ## Download results
  shinyjs::disable("ESC.Gene.download")
  observeEvent(ESC.Gene.overlap(), {
    shinyjs::enable("ESC.Gene.download")
  })
  
  output$ESC.Gene.download <- downloadHandler(
    filename = "ESC.Gene.download.csv",
    content = function(file) {
      write.csv(ESC.Gene.overlap(), file, row.names = FALSE,sep="\t")
    }
  )
  
  ####  
  #### Enhancer Phenotype table sub-panel
  ####
  
  ## Enhancer Phenotype table / select phenotype source
  
  ESC.Phenotype_table <- reactive({
    if (input$ESC.Phenotype.Choose.source == "OMIM") { 
      ESC.Phenotype_table <-ESC_OMIM %>% unique()%>% data.table() %>% setkey(chr, start, end)
    } else if (input$ESC.Phenotype.Choose.source == "GWAS"){
      ESC.Phenotype_table <- ESC_GWAS %>% unique()%>% data.table() %>% setkey(chr, start, end)
    } else  ESC.Phenotype_table <-  ESC_Clinvar %>% unique()%>% data.table() %>% setkey(chr, start, end)
  })
  
  output$ESC.Phenotype_table <- renderDataTable({
    if (input$ESC.Phenotype.Choose.source == "OMIM") { 
      datatable(ESC.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('GeneID and GeneName:'), "Enhancer target Ensembl GeneID and Gene Name (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('OMIM Gene Number:'), "OMIM Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('Phenotypes:'), "OMIM phenotype", tags$br (),style = "color:black;"),
                                                    htmltools::tags$span (strong ('OMIM Gene link:'), "Link to OMIM database", tags$br (), style = "color:black;"),     
                                                    style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
    } else if (input$ESC.Phenotype.Choose.source == "GWAS"){
      datatable(ESC.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('rsId:'), "ID of SNP associated with trait", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('Phenotypes:'), "GWAS phenotype", tags$br (),style = "color:black;"),
                                                    htmltools::tags$span (strong ('GeneName (GWAS):'), "GWAS Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('riskAllele:'), "Strongest SNP-risk allele", tags$br (), style = "color:black;"),  
                                                    htmltools::tags$span (strong ('riskAlFreq:'), "Risk allele frequency", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('pValue:'), "GWAS significant p-value", tags$br (), style = "color:black;"),  
                                                    htmltools::tags$span (strong ('pubMedID.link:'), "Link to PubMed ID of publication of the study", tags$br (), style = "color:black;"),     
                                                    style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
    } else  ESC.Phenotype_table <-  
        datatable(ESC.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                  caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('name:'), "Name of item", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('clinSign:'), "Clinical significance", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('GeneName (ClinVar):'), "ClinVar Gene related to phenotype", tags$br (),style = "color:black;"),
                                                      htmltools::tags$span (strong ('molConseq:'), "Molecular Consequence Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                      htmltools::tags$span (strong ('snpId:'), "dbSNP ID", tags$br (), style = "color:black;"),  
                                                      htmltools::tags$span (strong ('phenotype:'), "ClinVar phenotype", tags$br (), style = "color:black;"),                      
                                                      htmltools::tags$span (strong ('ClinVarId:'), "ClinVar variant ID", tags$br (), style = "color:black;"),  
                                                      htmltools::tags$span (strong ('ClinVarId.link:'), "Link to ClinVar database", tags$br (), style = "color:black;"),                      
                                                      style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })

  ## query file/text
  user_query.ESC.Phenotype <- reactive({
    if (input$ESCPhenotypeQueryChoose == 'file') { 
      req(input$ESC.Phenotype.query.file)
      ext <- tools::file_ext(input$ESC.Phenotype.query.file$name)
      switch(ext,
             bed = fread(input$ESC.Phenotype.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$ESC.Phenotype.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with enhancer data
  ESC.Phenotype.overlap <- eventReactive(input$ESC.Phenotype.run, {
    req(input$ESC.Phenotype.run)
      ESC.Phenotype.query.overlap<- foverlaps(user_query.ESC.Phenotype(), ESC.Phenotype_table () , nomatch = 0) %>% 
        unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
      ESC.Phenotype.query.overlap <- ESC.Phenotype.query.overlap %>% 
        mutate(Overlap.start = ESC.Phenotype.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
        mutate(Overlap.end = ESC.Phenotype.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
        mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
  })
  
  ## make variable for overlap table
  v_ESC.Phenotype <- reactiveValues(table=NULL)
  
  observeEvent(input$ESC.Phenotype.run, {
    req(input$ESC.Phenotype.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      ESC.Phenotype.overlap<-ESC.Phenotype.overlap()%>% unique()
    })
    
    v_ESC.Phenotype$table <- req(DT::datatable(ESC.Phenotype.overlap (),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
  },ignoreInit = TRUE)
  
  ## output
  output$ESC.Phenotype_overlap_table <- renderDT({ v_ESC.Phenotype$table }) 
  
  ## Download results
  shinyjs::disable("ESC.Phenotype.download")
  observeEvent(ESC.Phenotype.overlap(), {
    shinyjs::enable("ESC.Phenotype.download")
  })
  
  output$ESC.Phenotype.download <- downloadHandler(
    filename = "ESC.Phenotype.download.csv",
    content = function(file) {
      write.csv(ESC.Phenotype.overlap(), file, row.names = FALSE,sep="\t")
    }
  )
  
  #####  
  ##### visualization sub-panel
  ##### 
  
  ###
  ### Expression plot
  ###
  
  ## make query value
  ESC.Expression.gene.query <- eventReactive(input$ESC.Expression.plot, {
    unlist(strsplit(as.character(input$ESC.Expression.Gene), '\n', fixed=TRUE))
  }, ignoreNULL= T)
  
  ## extract query value from gene list
  ESC.Expression_plot<-reactive({ ESC.Expression.melt %>% 
      subset (GeneName %in% c(toupper(ESC.Expression.gene.query()))) %>% unique() 
  })
  
  ESC_Expression_table <- reactive({
    data.frame(fread('ESC_Enhancer/ESC_Expression.RPKM_hg19.bed'))%>%
      subset (GeneName %in% c(toupper(ESC.Expression.gene.query())))%>% unique()
  })
  
  ## make variable for query table and plot
  v_ESC.expression <- reactiveValues(plot= NULL, table=NULL)
  
  observeEvent(input$ESC.Expression.plot, {
    req(input$ESC.Expression.plot)
    withProgress(message = 'Analysis in progress', value = 0, {
      if (input$ESC.Expression.Choose.plot == "Heatmap plot") { 
        v_ESC.expression$plot <- ggplot2::ggplot(ESC.Expression_plot (), aes(x=variable, y=GeneName,fill=Log2_RPKM)) +
          geom_tile(color = "black") +
          scale_fill_gradient(low = "white", high = "red",name = "Log2 (Expression +1)") +
          coord_fixed()+
          theme(axis.text.x = element_text(size=12,color = "black",angle=90, hjust = 1,vjust=0.3),
                axis.text.y =element_text(size=12,color = "black"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank())
      } else if (input$ESC.Expression.Choose.plot == "Line plot"){
        v_ESC.expression$plot <- ggplot2::ggplot(ESC.Expression_plot (), aes(x=variable, y=Log2_RPKM, group=GeneName, colour=GeneName)) +
          geom_line() + geom_point()+
          theme_classic()+ 
          scale_y_continuous(limits = c(0, NA),"Log2 (Expression +1)",expand = c(0, 0.15))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.3,color='Black', size=12),
                axis.text.y=element_text(size=12, color="black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=12, color="black"))
      } else v_ESC.expression$plot <- ggplot2::ggplot(ESC.Expression_plot (), aes(x=variable, y=Log2_RPKM, group=GeneName, fill=GeneName)) +
          geom_bar(position="dodge", stat="identity")+
          theme_classic()+ 
          scale_y_continuous("Log2 (Expression +1)",expand = c(0, 0.15))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.3,color='Black', size=12),
                axis.text.y=element_text(size=12, color="black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=12, color="black"))
    })
    
    v_ESC.expression$table <- req(DT::datatable(ESC_Expression_table(),options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10),
                                                caption = htmltools::tags$caption ( htmltools::tags$span ("Expression level is based on RPKM/FPKM.", tags$br (), style = "color:black;"),
                                                                                    htmltools::tags$span (a(href="https://figshare.com/articles/dataset/SupplementaryTable_1/20533011","Click here, to know about expression data", tags$br (), style = "color:black;")),
                                                                                    style = 'caption-side: bottom;'),rownames = FALSE))
  },ignoreInit = TRUE)

  
  ##  output
  output$ESC.Expression_plot <- renderPlot({ v_ESC.expression$plot })
  output$ESC.Expression_table <- renderDT({ v_ESC.expression$table })
  
  ## Download results
  shinyjs::disable("ESC.Expression.download")
  observeEvent(ESC_Expression_table(), {
    shinyjs::enable("ESC.Expression.download")
  })
  
  output$ESC.Expression.download <- downloadHandler(
    filename = "ESC.Expression.RPKM_Table.csv",
    content = function(file) {
      write.csv(ESC_Expression_table(), file, row.names = FALSE,sep="\t")
    }
  )
  
  ###
  ### pLI score
  ###
  
  ## pLI plot
  
  ESC_pLI.gene.query <- eventReactive(input$ESC.pLI.plot, {
    unlist(strsplit(as.character(input$ESC.pLI.Gene), '\n', fixed=TRUE))
  }, ignoreNULL= T)
  
  ESC_pLI_plot <- reactive({
    read.csv('External_Data/pLI.score.csv') %>% mutate(pLI=round(pLI,digits = 4)) %>% subset (gene %in% c(toupper(ESC_pLI.gene.query())))%>% unique()
  })

  v_ESC_pLI <- reactiveValues(plot = NULL)
  
  observeEvent(input$ESC.pLI.plot, {
    v_ESC_pLI$plot <-ggplot2::ggplot(ESC_pLI_plot (), aes(x=gene, y=pLI)) +
      geom_bar(stat="identity", position=position_dodge(),fill="darkgray")+
      theme_classic()+ 
      geom_text(aes(label=pLI), vjust= -1, color="red",position = position_dodge(0.8), size=3.5)+
      scale_y_continuous("pLI score", expand = c(0, 0),limits = c(0,1.1),breaks=seq(0, 1, by = 0.2)) +
      labs(title="Probability of loss-of-function intolerance (pLI)", x="", y="",
           caption ="
           pLI defines the probability of a gene being intolerant to variation causing loss of gene function.
           It is frequently used to prioritize candidate genes when analyzing whole exome or whole genome data. 
           It is ranged from 0 to 1 and in total, 3230 genes are identified as intolerant (pLI > 0.9) and 10374 as tolerant (pLI <0.1).
           
           pLI score is obtained from:           
           Lek M, et al. 2016. Exome Aggregation Consortium. Analysis of protein-coding genetic variation in 60,706 humans.PMID: 27535533.") +
      theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1,color='Black', size=12),
            axis.text.y=element_text(size=12, color="black"),
            plot.title = element_text(hjust = 0.5, size = 15,face = "bold"),
            plot.caption = element_text(hjust = 0,size=14,lineheight = 1.3))
  },ignoreInit = TRUE)
  
  ##output
  output$ESC.pLI_plot <- renderPlot({ v_ESC_pLI$plot })
  
  ## Download results
  shinyjs::disable("ESC.pLI.download")
  observeEvent(ESC_pLI_plot(), {
    shinyjs::enable("ESC.pLI.download")
  })
  
  output$ESC.pLI.download <- downloadHandler(
    filename = "pLI.Scores_Table.csv",
    content = function(file) {
      write.csv(ESC_pLI_plot(), file, row.names = FALSE,sep="\t")
    }
  )
  
  ###
  ### DNA sequence features
  ###
  ## query file/text
  user_query.ESC.DNA.features <- reactive({
    if (input$ESCDNAfeaturesQueryChoose == 'file') { 
      req(input$ESC.DNA.features.query.file)
      ext <- tools::file_ext(input$ESC.DNA.features.query.file$name)
      switch(ext,
             bed = fread(input$ESC.DNA.features.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$ESC.DNA.features.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with PCR_DNA_features
  ESC.DNA.features.overlap <- eventReactive(input$ESC.DNA.features.run, {
    req(input$ESC.DNA.features.run)
      ESC.DNA.features.overlap <- foverlaps(user_query.ESC.DNA.features(), ESC_DNA.features.table , nomatch = 0) %>% 
        unique() %>% setkey(chr, start, end) %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)%>% unique()
  })

  ## make variable for overlap table and plot
  v_ESC_DNA.features <- reactiveValues(plot = NULL,table=NULL)
  
  observeEvent(input$ESC.DNA.features.run, {
    
    req(input$ESC.DNA.features.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      
      ESC_DNA.features_overlap <-  ESC.DNA.features.overlap () %>%dplyr::select(chr,start,end,GC.content,phastCons.score,nCER.score,LoF.tolerant.score) %>%
        reshape2 :: melt(id= c("chr","start","end"))%>% unique()%>% group_by(variable)%>%arrange (desc(value), .by_group = TRUE)%>% data.table()
    })
    
    ESC_DNA.features_plot <-  egg::ggarrange  ( ggplot2::ggplot(ESC_DNA.features_overlap %>% subset(variable %in% "GC.content")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray28")+
                                                   theme_classic()+
                                                   scale_x_continuous("GC content", expand = c(0, 0)) +
                                                   labs(y="Enhancer")  ,
                                                 
                                                 ggplot2::ggplot(ESC_DNA.features_overlap %>% subset(variable %in% "phastCons.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray47")+
                                                   theme_classic()+
                                                   scale_x_continuous("phastCons score", expand = c(0, 0)) +
                                                   labs(y="Enhancer"),
                                                 
                                                 ggplot2::ggplot(ESC_DNA.features_overlap %>% subset(variable %in% "nCER.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray68")+
                                                   theme_classic()+
                                                   scale_x_continuous("ncER score", expand = c(0, 0)) +
                                                   labs(y="Enhancer"),
                                                 
                                                 ggplot2::ggplot(ESC_DNA.features_overlap %>% subset(variable %in% "LoF.tolerant.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray84")+
                                                   theme_classic()+
                                                   scale_x_continuous("LoF tolerant score", expand = c(0, 0)) +
                                                   labs(y="Enhancer") ,
                                                 
                                                 ncol = 4 ,nrow = 1)
    v_ESC_DNA.features$plot <- ggpubr::annotate_figure(ESC_DNA.features_plot, top = ggpubr::text_grob("Plots show only top 30 enriched regions", color = "red", face = "bold", size = 16))
    v_ESC_DNA.features$table <- req(DT::datatable(ESC.DNA.features.overlap(),options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
    
  },ignoreInit = TRUE)
  
  
  ## output
  output$ESC.DNA.features_plot <- renderPlot({ v_ESC_DNA.features$plot }, height = 400, width = 1500)
  output$ESC.DNA.features_table <- renderDT({ v_ESC_DNA.features$table }) 
  
  ## Download results
  shinyjs::disable("ESC.DNA.features.download")
  observeEvent(ESC.DNA.features.overlap(), {
    shinyjs::enable("ESC.DNA.features.download")
  })
  
  output$ESC.DNA.features.download <- downloadHandler(
    filename = "DNA.feature_Table.csv",
    content = function(file) {
      write.csv(ESC.DNA.features.overlap(), file, row.names = FALSE,sep="\t")
    }
  )
  
  ## UCSC ESC sample Image 
  output$ESC_UCSC_sample <- renderImage({
    list(src = "www/ESC_UCSC.jpg",
         alt = "ESC UCSC sample",
         width=860,height=800)
  }, deleteFile = F)
  
  ####################
  #################### NSC_panel
  ####################
  
  ### NSC Enhancer Table sub-panel
  NSC_table <- reactive({
    if (input$NSC.table.allrank == "All enhancer activity") { 
      NSC_table <-  NSC.enhancer_Vista %>% unique() %>% dplyr::filter(CellType %in% input$NSC.table.celltype) %>% unique() 
    }  else NSC_table <- dplyr::filter(NSC.enhancer_Vista, between(`Activity Rank`, input$NSC.table.rank[1], input$NSC.table.rank[2]), CellType %in%  input$NSC.table.celltype)%>% unique() 
    return(NSC_table)
  })
  

  output$NSC_table <- renderDataTable({
    datatable(NSC_table (), escape = FALSE, options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = FALSE),
              caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr, star, end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                  #htmltools::tags$span (strong ('Activity Score:'), "Enhancer Activity Score based on STARR-seq data (RNA/DNA ratio)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Activity Rank:'), "Enhancer activity level: the most active (Rank 10) and the least active (Rank 1)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Cell type:'), "NSC ChIP-STARR-seq data in NSC/ESC cells", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('Enhancer activity (Vista):'), "Overlap between enhancer table and positive validated Vista enhancers", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('Expression pattern (Vista):'), "Expression pattern of validated Vista enhancers", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('VistaID:'), "Vista enhancer ID", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('VistaID.link:'), "Check enhancer in Vista Enhancer browser", style = "color:black;"),
                                                  style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  
  ##  Adjust to upload file up to 75MB (Default is 5 MB)
  options(shiny.maxRequestSize=75*1024^2)
  
  ## query file/text
  user_query.NSC <- reactive({
    if (input$NSCTableQueryChoose == 'file') { 
      req(input$NSC.Table.query.file)
      ext <- tools::file_ext(input$NSC.Table.query.file$name)
      switch(ext,
             bed = fread(input$NSC.Table.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$NSC.Table.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with enhancer data
  NSC.overlap <- eventReactive(input$NSC.run, {
    req(input$NSC.run)
    NSC.query.overlap<- foverlaps(user_query.NSC(), NSC_table () , nomatch = 0) %>% 
      unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
    NSC.query.overlap <- NSC.query.overlap %>% 
      mutate(Overlap.start = NSC.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
      mutate(Overlap.end = NSC.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
      mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
    
  })
  
  ## make variable for overlap table and plot
  v_NSC.Enhancer <- reactiveValues(plot=NULL,table=NULL)
  
  observeEvent(input$NSC.run, {
    req(input$NSC.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      NSC.overlap<-NSC.overlap()%>% unique()
    })
    
    v_NSC.Enhancer$plot <-VennDiagram::draw.pairwise.venn(
      area1 = nrow(unique(user_query.NSC()[,1:3])),
      area2 = nrow(unique(NSC_table()[,1:3])),
      cross.area = nrow(unique(NSC.overlap() [,c(1,7:8)])),
      category = c("Query", "Enhancer"), 
      col = c("black", "darkgray"),
      cex = 1.5,
      cat.cex = 2,
      euler.d =T,
      scaled=F,
      ext.text=T,
      sep.dist = 0.03,
      cat.pos = c(180, 180))
    
    v_NSC.Enhancer$table <- req(DT::datatable(NSC.overlap(),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
    
  },ignoreInit = TRUE)
  
  ## output
  output$NSC_overlap_table <- renderDT({ v_NSC.Enhancer$table }) 
  output$NSC_overlap_plot  <- renderPlot({ gridExtra::grid.arrange(gTree(children=v_NSC.Enhancer$plot)) })
  
  ## Download results
  shinyjs::disable("NSC.download")
  observeEvent(NSC.overlap(), {
    shinyjs::enable("NSC.download")
  })
  
  output$NSC.download <- downloadHandler(
    filename = "NSC.Table.download.csv",
    content = function(file) {
      write.csv(NSC.overlap(), file, row.names = FALSE)
    }
  )
  
  ####
  #### Enhancer Gene table sub-panel
  ####
  
  ## Enhancer gene table
  
  NSC.Gene_table <- reactive({
    if (input$NSC.Gene.allrank == "All enhancer activity") { 
      NSC.Gene_table <- NSC_Gene %>% unique() %>% dplyr::filter(CellType %in% input$NSC.Gene.celltype) %>% unique() 
      } else NSC.Gene_table <- dplyr::filter(NSC_Gene, between(`Activity Rank`, input$NSC.Gene.rank[1], input$NSC.Gene.rank[2]), CellType %in%  input$NSC.Gene.celltype)%>% unique() 
      return(NSC.Gene_table) 
      })
  
  output$NSC.Gene_table<- renderDataTable({
    datatable(NSC.Gene_table (), options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
              caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr, start, end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                  #htmltools::tags$span (strong ('Activity Score:'), "Enhancer Activity Score based on STARR-seq data (RNA/DNA ratio)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Activity Rank:'), "Enhancer activity level: the most active (Rank 10) and the least active (Rank 1)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('GeneID and GeneName:'), "Target gene (hg19)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Cell type:'), "NSC ChIP-STARR-seq data in NSC/ESC cells", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('GeneDefine:'), "TSS (defined from -1 kb to +100 bp), HiC (based on 25 Kb resolution bins), JEME and GeneHancer", style = "color:black;"),
                                                  style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  ## query file/text
  user_query.NSC.Gene <- reactive({
    if (input$NSCGeneQueryChoose == 'file') { 
      req(input$NSC.Gene.query.file)
      ext <- tools::file_ext(input$NSC.Gene.query.file$name)
      switch(ext,
             bed = fread(input$NSC.Gene.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$NSC.Gene.query.text)) %>% setkey(chr, start, end)
  })
  
  ## overlap query file/text with enhancer data
  NSC.Gene.overlap <- eventReactive(input$NSC.Gene.run, {
    req(input$NSC.Gene.run)
    NSC.Gene.query.overlap<- foverlaps(user_query.NSC.Gene(), NSC.Gene_table () , nomatch = 0) %>% 
      unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
    NSC.Gene.query.overlap <- NSC.Gene.query.overlap %>% 
      mutate(Overlap.start = NSC.Gene.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
      mutate(Overlap.end = NSC.Gene.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
      mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
  })
  
  ## make variable for overlap table
  v_NSC.Gene <- reactiveValues(table=NULL)
  
  observeEvent(input$NSC.Gene.run, {
    req(input$NSC.Gene.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      NSC.Gene.overlap<-NSC.Gene.overlap()%>% unique()
    })
    
    v_NSC.Gene$table <- req(DT::datatable(NSC.Gene.overlap(),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
  },ignoreInit = TRUE)
  
  ## output
  output$NSC.Gene_overlap_table <- renderDT({ v_NSC.Gene$table }) 
  
  
  ## Download results
  shinyjs::disable("NSC.Gene.download")
  observeEvent(NSC.Gene.overlap(), {
    shinyjs::enable("NSC.Gene.download")
  })
  
  output$NSC.Gene.download <- downloadHandler(
    filename = "NSC.Gene.download.csv",
    content = function(file) {
      write.csv(NSC.Gene.overlap(), file, row.names = FALSE)
    }
  )
  
  ####  
  #### Enhancer Phenotype table sub-panel
  ####
  
  NSC.Phenotype_table <- reactive({
    if (input$NSC.Phenotype.Choose.source == "OMIM") { 
      NSC.Phenotype_table <-NSC_OMIM %>% unique()%>% data.table() %>% setkey(chr, start, end)
    } else if (input$NSC.Phenotype.Choose.source == "GWAS"){
      NSC.Phenotype_table <- NSC_GWAS %>% unique()%>% data.table() %>% setkey(chr, start, end)
    } else  NSC.Phenotype_table <-  NSC_Clinvar %>% unique()%>% data.table() %>% setkey(chr, start, end)
  })
  
  
  output$NSC.Phenotype_table <- renderDataTable({
    if (input$NSC.Phenotype.Choose.source == "OMIM") { 
      datatable(NSC.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('GeneID and GeneName:'), "Enhancer target Ensembl GeneID and Gene Name (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('OMIM Gene Number:'), "OMIM Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('Phenotypes:'), "OMIM phenotype", tags$br (),style = "color:black;"),
                                                    htmltools::tags$span (strong ('OMIM Gene link:'), "Link to OMIM database", tags$br (), style = "color:black;"),     
                                                    style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
      
    } else if (input$NSC.Phenotype.Choose.source == "GWAS"){
      datatable(NSC.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('rsId:'), "ID of SNP associated with trait", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('Phenotypes:'), "GWAS phenotype", tags$br (),style = "color:black;"),
                                                    htmltools::tags$span (strong ('GeneName (GWAS):'), "GWAS Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('riskAllele:'), "Strongest SNP-risk allele", tags$br (), style = "color:black;"),  
                                                    htmltools::tags$span (strong ('riskAlFreq:'), "Risk allele frequency", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('pValue:'), "GWAS significant p-value", tags$br (), style = "color:black;"),  
                                                    htmltools::tags$span (strong ('pubMedID.link:'), "Link to PubMed ID of publication of the study", tags$br (), style = "color:black;"),     
                                                    style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
      
      
    } else  NSC.Phenotype_table <-  
        datatable(NSC.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                  caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('name:'), "Name of item", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('clinSign:'), "Clinical significance", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('GeneName (ClinVar):'), "ClinVar Gene related to phenotype", tags$br (),style = "color:black;"),
                                                      htmltools::tags$span (strong ('molConseq:'), "Molecular Consequence Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                      htmltools::tags$span (strong ('snpId:'), "dbSNP ID", tags$br (), style = "color:black;"),  
                                                      htmltools::tags$span (strong ('phenotype:'), "ClinVar phenotype", tags$br (), style = "color:black;"),                      
                                                      htmltools::tags$span (strong ('ClinVarId:'), "ClinVar variant ID", tags$br (), style = "color:black;"),  
                                                      htmltools::tags$span (strong ('ClinVarId.link:'), "Link to ClinVar database", tags$br (), style = "color:black;"),                      
                                                      style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  
  user_query.NSC.Phenotype <- reactive({
    if (input$NSCPhenotypeQueryChoose == 'file') { 
      req(input$NSC.Phenotype.query.file)
      ext <- tools::file_ext(input$NSC.Phenotype.query.file$name)
      switch(ext,
             bed = fread(input$NSC.Phenotype.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$NSC.Phenotype.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with enhancer data
  NSC.Phenotype.overlap <- eventReactive(input$NSC.Phenotype.run, {
    req(input$NSC.Phenotype.run)
      NSC.Phenotype.query.overlap<- foverlaps(user_query.NSC.Phenotype(), NSC.Phenotype_table () , nomatch = 0) %>% 
        unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
      NSC.Phenotype.query.overlap <- NSC.Phenotype.query.overlap %>% 
        mutate(Overlap.start = NSC.Phenotype.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
        mutate(Overlap.end = NSC.Phenotype.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
        mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
  })
  
  ## make variable for overlap table
  v_NSC.Phenotype <- reactiveValues(table=NULL)
  
  observeEvent(input$NSC.Phenotype.run, {
    req(input$NSC.Phenotype.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      NSC.Phenotype.overlap<-NSC.Phenotype.overlap()%>% unique()
    })
    
    v_NSC.Phenotype$table <- req(DT::datatable(NSC.Phenotype.overlap (),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
  },ignoreInit = TRUE)
  
  ## output
  output$NSC.Phenotype_overlap_table <- renderDT({ v_NSC.Phenotype$table }) 
  
  ## Download results
  shinyjs::disable("NSC.Phenotype.download")
  observeEvent(NSC.Phenotype.overlap(), {
    shinyjs::enable("NSC.Phenotype.download")
  })
  
  output$NSC.Phenotype.download <- downloadHandler(
    filename = "NSC.Phenotype.download.csv",
    content = function(file) {
      write.csv(NSC.Phenotype.overlap(), file, row.names = FALSE)
    }
  )
  
  #####  
  ##### visualization sub-panel
  ##### 
  
  ###
  ### Expression plot
  ###
  
  ## make query value
  NSC.Expression.gene.query <- eventReactive(input$NSC.Expression.plot, {
    unlist(strsplit(as.character(input$NSC.Expression.Gene), '\n', fixed=TRUE))
  }, ignoreNULL= T)
  
  ## overlap between gene list and query value
  NSC.Expression_plot<-reactive({ NSC.Expression.melt %>% 
      subset (GeneName %in% c(toupper(NSC.Expression.gene.query()))) %>% unique() 
  })

  NSC_Expression_table <- reactive({
    data.frame(fread('NSC_Enhancer/Expression.RPKM_cells_hg19.bed'))%>%
      subset (GeneName %in% c(toupper(NSC.Expression.gene.query())))%>% unique()
  })
  
  ## make variable for query table and plot
  v_NSC.expression <- reactiveValues(plot= NULL, table=NULL)
  
  observeEvent(input$NSC.Expression.plot, {
    req(input$NSC.Expression.plot)
    withProgress(message = 'Analysis in progress', value = 0, {
      if (input$NSC.Expression.Choose.plot == "Heatmap plot") { 
        v_NSC.expression$plot <- ggplot2::ggplot(NSC.Expression_plot (), aes(x=variable, y=GeneName,fill=Log2_RPKM)) +
          geom_tile(color = "black") +
          scale_fill_gradient(low = "white", high = "red",name = "Log2 (Expression +1)") +
          coord_fixed()+
          theme(axis.text.x = element_text(size=12,color = "black",angle=90, hjust = 1,vjust=0.3),
                axis.text.y =element_text(size=12,color = "black"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank())
      } else if (input$NSC.Expression.Choose.plot == "Line plot"){
        v_NSC.expression$plot <- ggplot2::ggplot(NSC.Expression_plot (), aes(x=variable, y=Log2_RPKM, group=GeneName, colour=GeneName)) +
          geom_line() + geom_point()+
          theme_classic()+ 
          scale_y_continuous(limits = c(0, NA),"Log2 (Expression +1)",expand = c(0, 0.15))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.3,color='Black', size=12),
                axis.text.y=element_text(size=12, color="black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=12, color="black"))
      } else v_NSC.expression$plot <- ggplot2::ggplot(NSC.Expression_plot (), aes(x=variable, y=Log2_RPKM, group=GeneName, fill=GeneName)) +
          geom_bar(position="dodge", stat="identity")+
          theme_classic()+ 
          scale_y_continuous("Log2 (Expression +1)",expand = c(0, 0.15))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.3,color='Black', size=12),
                axis.text.y=element_text(size=12, color="black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=12, color="black"))
    })
    
    v_NSC.expression$table <- req(DT::datatable(NSC_Expression_table(),options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10),
                                                caption = htmltools::tags$caption ( htmltools::tags$span ("Expression level is based on RPKM/FPKM.", tags$br (), style = "color:black;"),
                                                                                    htmltools::tags$span (a(href="https://figshare.com/articles/dataset/SupplementaryTable_1/20533011","Click here, to know about expression data", tags$br (), style = "color:black;")),
                                                                                    style = 'caption-side: bottom;'),rownames = FALSE))
  },ignoreInit = TRUE)
  
  
  ##  output
  output$NSC.Expression_plot <- renderPlot({ v_NSC.expression$plot })
  output$NSC.Expression_table <- renderDT({ v_NSC.expression$table })
  
  ## Download results
  shinyjs::disable("NSC.Expression.download")
  observeEvent(NSC_Expression_table(), {
    shinyjs::enable("NSC.Expression.download")
  })
  
  output$NSC.Expression.download <- downloadHandler(
    filename = "NSC.Expression_Table.csv",
    content = function(file) {
      write.csv(NSC_Expression_table(), file, row.names = FALSE)
    }
  )
  
  ###
  ### pLI score
  ###
  
  ## pLI plot
  
  NSC.pLI.gene.query <- eventReactive(input$NSC.pLI.plot, {
    unlist(strsplit(as.character(input$NSC.pLI.Gene), '\n', fixed=TRUE))
  }, ignoreNULL= T)
  
  NSC_pLI_plot <- reactive({
    read.csv('External_Data/pLI.score.csv') %>% mutate(pLI=round(pLI,digits = 4)) %>% subset (gene %in% c(toupper(NSC.pLI.gene.query()))) %>% unique()
  })
  
  v_NSC_pLI <- reactiveValues(plot = NULL)
  
  observeEvent(input$NSC.pLI.plot, {
    v_NSC_pLI$plot <-ggplot2::ggplot(NSC_pLI_plot (), aes(x=gene, y=pLI)) +
      geom_bar(stat="identity", position=position_dodge(),fill="darkgray")+
      theme_classic()+ 
      geom_text(aes(label=pLI), vjust= -1, color="red",position = position_dodge(0.8), size=3.5)+
      scale_y_continuous("pLI score", expand = c(0, 0),limits = c(0,1.1),breaks=seq(0, 1, by = 0.2)) +
      labs(title="Probability of loss-of-function intolerance (pLI)", x="", y="",
           caption ="
           pLI defines the probability of a gene being intolerant to variation causing loss of gene function.
           It is frequently used to prioritize candidate genes when analyzing whole exome or whole genome data. 
           It is ranged from 0 to 1 and in total, 3230 genes are identified as intolerant (pLI > 0.9) and 10374 as tolerant (pLI <0.1).
           
           pLI score is obtained from:           
           Lek M, et al. 2016. Exome Aggregation Consortium. Analysis of protein-coding genetic variation in 60,706 humans.PMID: 27535533.") +
      theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1,color='Black', size=12),
            axis.text.y=element_text(size=12, color="black"),
            plot.title = element_text(hjust = 0.5, size = 15,face = "bold"),
            plot.caption = element_text(hjust = 0,size=14,lineheight = 1.3))
  },ignoreInit = TRUE)
  
  ##output
  output$NSC.pLI_plot <- renderPlot({ v_NSC_pLI$plot })
  
  ## Download results
  shinyjs::disable("NSC.pLI.download")
  observeEvent(NSC_pLI_plot(), {
    shinyjs::enable("NSC.pLI.download")
  })
  
  output$NSC.pLI.download <- downloadHandler(
    filename = "pLI.Scores_Table.csv",
    content = function(file) {
      write.csv(NSC_pLI_plot(), file, row.names = FALSE)
    }
  )
  
  ###
  ### DNA sequence features
  ###
  
  ## query file/text
  user_query.NSC.DNA.features <- reactive({
    if (input$NSCDNAfeaturesQueryChoose == 'file') { 
      req(input$NSC.DNA.features.query.file)
      ext <- tools::file_ext(input$NSC.DNA.features.query.file$name)
      switch(ext,
             bed = fread(input$NSC.DNA.features.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$NSC.DNA.features.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with PCR_DNA_features
  NSC.DNA.features.overlap <- eventReactive(input$NSC.DNA.features.run, {
    req(input$NSC.DNA.features.run)
      NSC.DNA.features.overlap <- foverlaps(user_query.NSC.DNA.features(), NSC_DNA.features.table , nomatch = 0) %>% 
        unique() %>% setkey(chr, start, end) %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)%>% unique()
  })
  
  ## make variable for overlap table and plot
  v_NSC_DNA.features <- reactiveValues(plot = NULL,table=NULL)
  
  observeEvent(input$NSC.DNA.features.run, {
    
    req(input$NSC.DNA.features.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      
      NSC_DNA.features_overlap <-  NSC.DNA.features.overlap () %>%dplyr::select(chr,start,end,GC.content,phastCons.score,nCER.score,LoF.tolerant.score) %>%
        reshape2 :: melt(id= c("chr","start","end"))%>% unique()%>% group_by(variable)%>%arrange (desc(value), .by_group = TRUE)%>% data.table()
    })
    
    NSC_DNA.features_plot <- egg::ggarrange  ( ggplot2::ggplot(NSC_DNA.features_overlap %>% subset(variable %in% "GC.content")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray28")+
                                                   theme_classic()+
                                                   scale_x_continuous("GC content", expand = c(0, 0)) +
                                                   labs(y="Enhancer")  ,
                                                 
                                                 ggplot2::ggplot(NSC_DNA.features_overlap %>% subset(variable %in% "phastCons.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray47")+
                                                   theme_classic()+
                                                   scale_x_continuous("phastCons score", expand = c(0, 0)) +
                                                   labs(y="Enhancer"),
                                                 
                                                 ggplot2::ggplot(NSC_DNA.features_overlap %>% subset(variable %in% "nCER.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray68")+
                                                   theme_classic()+
                                                   scale_x_continuous("ncER score", expand = c(0, 0)) +
                                                   labs(y="Enhancer"),
                                                 
                                                 ggplot2::ggplot(NSC_DNA.features_overlap %>% subset(variable %in% "LoF.tolerant.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray84")+
                                                   theme_classic()+
                                                   scale_x_continuous("LoF tolerant score", expand = c(0, 0)) +
                                                   labs(y="Enhancer") ,
                                                 
                                                 ncol = 4 ,nrow = 1)
    
    v_NSC_DNA.features$plot <- ggpubr::annotate_figure(NSC_DNA.features_plot, top = ggpubr::text_grob("Plots show only top 30 enriched regions", color = "red", face = "bold", size = 16))
    v_NSC_DNA.features$table <- req(DT::datatable(NSC.DNA.features.overlap(),options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
    
  },ignoreInit = TRUE)
  
  
  ## output
  output$NSC.DNA.features_plot <- renderPlot({ v_NSC_DNA.features$plot }, height = 400, width = 1500)
  output$NSC.DNA.features_table <- renderDT({ v_NSC_DNA.features$table }) 
  
  ## Download results
  shinyjs::disable("NSC.DNA.features.download")
  observeEvent(NSC.DNA.features.overlap(), {
    shinyjs::enable("NSC.DNA.features.download")
  })
  
  output$NSC.DNA.features.download <- downloadHandler(
    filename = "NSC.DNA.feature_Table.csv",
    content = function(file) {
      write.csv(NSC.DNA.features.overlap(), file, row.names = FALSE,sep="\t")
    }
  )
  
  ## UCSC NSC sample Image 
  output$NSC_UCSC_sample <- renderImage({
    list(src = "www/NSC_UCSC.jpg",
         alt = "NSC UCSC sample",
         width=900,height=800)
  }, deleteFile = F)
  
  ####################
  #################### Fetal.Brain_panel
  ####################
  
  ###
  ### Enhancer table sub-panel
  ###
  
  ## enhancer table
  FB_table <- reactive({
    if (input$FB.Enhancer.type == "DAE") { 
      FB_table <-  FB.enhancer_Vista %>% dplyr::filter(EnhancerType %in% input$FB.Enhancer.type) %>% unique() 
    } else if (input$FB.Enhancer.type == "nDAE") { 
      FB_table <-  FB.enhancer_Vista %>% dplyr::filter(EnhancerType %in% input$FB.Enhancer.type) %>% unique() 
    } else FB_table <-  FB.enhancer_Vista %>% unique()
  })
  
  output$FB_table <- renderDataTable({
    datatable(FB_table (), escape = FALSE, options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = FALSE),
              caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr, star, end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('DAE:'), "Differentially Active Enhancer", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('nDAE:'), "non-Differentially Active Enhancer",tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('Enhancer activity (Vista):'), "Overlap between enhancer table and positive validated Vista enhancers", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('Expression pattern (Vista):'), "Expression pattern of validated Vista enhancers", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('VistaID:'), "Vista enhancer ID", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('VistaID.link:'), "Check enhancer in Vista Enhancer browser", style = "color:black;"),
                                                  style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  
  ## Read uploaded user file
  ## query file/text
  user_query.FB <- reactive({
    if (input$FBTableQueryChoose == 'file') { 
      req(input$FB.Table.query.file)
      ext <- tools::file_ext(input$FB.Table.query.file$name)
      switch(ext,
             bed = fread(input$FB.Table.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$FB.Table.query.text)) %>% setkey(chr, start, end)
  })
  
  ## overlap query file/text with enhancer data
  FB.overlap <- eventReactive(input$FB.run, {
    req(input$FB.run)
    FB.query.overlap<- foverlaps(user_query.FB(), FB_table () , nomatch = 0) %>% 
      unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
    FB.query.overlap <- FB.query.overlap %>% 
      mutate(Overlap.start = FB.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
      mutate(Overlap.end = FB.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
      mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
    
  })
  
  ## make variable for overlapping table and plot
  v_FB.Enhancer <- reactiveValues(plot=NULL,table=NULL)
  
  observeEvent(input$FB.run, {
    req(input$FB.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      FB.overlap<-FB.overlap()%>% unique()
    })
    
    v_FB.Enhancer$plot <-VennDiagram::draw.pairwise.venn(
      area1 = nrow(unique(user_query.FB()[,1:3])),
      area2 = nrow(unique(FB_table()[,1:3])),
      cross.area = nrow(unique(FB.overlap() [,c(1,5:6)])),
      category = c("Query", "Enhancer"), 
      col = c("black", "darkgray"),
      cex = 1.5,
      cat.cex = 2,
      euler.d =T,
      scaled=F,
      ext.text=T,
      sep.dist = 0.03,
      cat.pos = c(180, 180))
    
    v_FB.Enhancer$table <- req(DT::datatable(FB.overlap(),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
    
  },ignoreInit = TRUE)
  
  
  ## output
  output$FB_overlap_table <- renderDT({ v_FB.Enhancer$table }) 
  output$FB_overlap_plot  <- renderPlot({ gridExtra::grid.arrange(gTree(children=v_FB.Enhancer$plot)) })
  
  ## Download results
  shinyjs::disable("FB.download")
  observeEvent(FB.overlap(), {
    shinyjs::enable("FB.download")
  })
  
  output$FB.download <- downloadHandler(
    filename = "FB.Table.download.csv",
    content = function(file) {
      write.csv(FB.overlap(), file, row.names = FALSE)
    }
  )
  
  ###
  ### Enhancer Gene table sub-panel
  ###
  
  FB.Gene_table <- reactive({
    if (input$FB.Gene.type == "DAE") { 
      FB.Gene_table <-  FB_Gene %>% dplyr::filter(EnhancerType %in% input$FB.Gene.type) %>% unique() 
    } else if (input$FB.Gene.type == "nDAE") { 
      FB.Gene_table <-  FB_Gene %>% dplyr::filter(EnhancerType %in% input$FB.Gene.type) %>% unique() 
    } else FB.Gene_table <-  FB_Gene %>% unique()
  })
  
  output$FB.Gene_table<- renderDataTable({
    datatable(FB.Gene_table (), options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
              caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr, start, end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('GeneID and GeneName:'), "Target gene (hg19)", tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('DAE:'), "Differentially Active Enhancer", tags$br (), style = "color:black;"),                      
                                                  htmltools::tags$span (strong ('nDAE:'), "non-Differentially Active Enhancer",tags$br (), style = "color:black;"),
                                                  htmltools::tags$span (strong ('GeneDefine:'), "TSS (defined from -1 kb to +100 bp), HiC (based on 25 Kb resolution bins), JEME and GeneHancer", style = "color:black;"),
                                                  style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  ## query file/text
  user_query.FB.Gene <- reactive({
    if (input$FBGeneQueryChoose == 'file') { 
      req(input$FB.Gene.query.file)
      ext <- tools::file_ext(input$FB.Gene.query.file$name)
      switch(ext,
             bed = fread(input$FB.Gene.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$FB.Gene.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with enhancer data
  FB.Gene.overlap <- eventReactive(input$FB.Gene.run, {
    req(input$FB.Gene.run)
    FB.Gene.query.overlap<- foverlaps(user_query.FB.Gene(), FB.Gene_table () , nomatch = 0) %>% 
      unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
    FB.Gene.query.overlap <- FB.Gene.query.overlap %>% 
      mutate(Overlap.start = FB.Gene.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
      mutate(Overlap.end = FB.Gene.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
      mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
  })
  
  ## make variable for overlap table
  v_FB.Gene <- reactiveValues(table=NULL)
  
  observeEvent(input$FB.Gene.run, {
    req(input$FB.Gene.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      FB.Gene.overlap<-FB.Gene.overlap()%>% unique()
    })
    
    v_FB.Gene$table <- req(DT::datatable(FB.Gene.overlap(),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
  },ignoreInit = TRUE)
  
  ## output
  output$FB.Gene_overlap_table <- renderDT({ v_FB.Gene$table }) 
  
  ## Download results
  shinyjs::disable("FB.Gene.download")
  observeEvent(FB.Gene.overlap(), {
    shinyjs::enable("FB.Gene.download")
  })
  
  output$FB.Gene.download <- downloadHandler(
    filename = "FB.Gene.download.csv",
    content = function(file) {
      write.csv(FB.Gene.overlap(), file, row.names = FALSE)
    }
  )
  
  ###  
  ### Enhancer Phenotype table sub-panel
  ###
  
  FB.Phenotype_table <- reactive({
    if (input$FB.Phenotype.Choose.source == "OMIM") { 
      FB.Phenotype_table <-FB_OMIM %>% unique()%>% data.table() %>% setkey(chr, start, end)
    } else if (input$FB.Phenotype.Choose.source == "GWAS"){
      FB.Phenotype_table <- FB_GWAS %>% unique()%>% data.table() %>% setkey(chr, start, end)
    } else  FB.Phenotype_table <-  FB_Clinvar %>% unique()%>% data.table() %>% setkey(chr, start, end)
  })
  
  
  output$FB.Phenotype_table <- renderDataTable({
    if (input$FB.Phenotype.Choose.source == "OMIM") { 
      datatable(FB.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('GeneID and GeneName:'), "Enhancer target Ensembl GeneID and Gene Name (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('OMIM Gene Number:'), "OMIM Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('Phenotypes:'), "OMIM phenotype", tags$br (),style = "color:black;"),
                                                    htmltools::tags$span (strong ('OMIM Gene link:'), "Link to OMIM database", tags$br (), style = "color:black;"),     
                                                    style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
      
    } else if (input$FB.Phenotype.Choose.source == "GWAS"){
      datatable(FB.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('rsId:'), "ID of SNP associated with trait", tags$br (), style = "color:black;"),
                                                    htmltools::tags$span (strong ('Phenotypes:'), "GWAS phenotype", tags$br (),style = "color:black;"),
                                                    htmltools::tags$span (strong ('GeneName (GWAS):'), "GWAS Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('riskAllele:'), "Strongest SNP-risk allele", tags$br (), style = "color:black;"),  
                                                    htmltools::tags$span (strong ('riskAlFreq:'), "Risk allele frequency", tags$br (), style = "color:black;"),                      
                                                    htmltools::tags$span (strong ('pValue:'), "GWAS significant p-value", tags$br (), style = "color:black;"),  
                                                    htmltools::tags$span (strong ('pubMedID.link:'), "Link to PubMed ID of publication of the study", tags$br (), style = "color:black;"),     
                                                    style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
      
      
    } else  FB.Phenotype_table <-  
        datatable(FB.Phenotype_table (), escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10, searching = T),
                  caption = htmltools::tags$caption ( htmltools::tags$span (strong ('chr,start,end:'), "Enhancer coordinate (hg19)", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('name:'), "Name of item", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('clinSign:'), "Clinical significance", tags$br (), style = "color:black;"),
                                                      htmltools::tags$span (strong ('GeneName (ClinVar):'), "ClinVar Gene related to phenotype", tags$br (),style = "color:black;"),
                                                      htmltools::tags$span (strong ('molConseq:'), "Molecular Consequence Gene related to phenotype", tags$br (), style = "color:black;"),                      
                                                      htmltools::tags$span (strong ('snpId:'), "dbSNP ID", tags$br (), style = "color:black;"),  
                                                      htmltools::tags$span (strong ('phenotype:'), "ClinVar phenotype", tags$br (), style = "color:black;"),                      
                                                      htmltools::tags$span (strong ('ClinVarId:'), "ClinVar variant ID", tags$br (), style = "color:black;"),  
                                                      htmltools::tags$span (strong ('ClinVarId.link:'), "Link to ClinVar database", tags$br (), style = "color:black;"),                      
                                                      style = 'caption-side: bottom;'),selection = 'single',rownames = FALSE)
  })
  
  
  ## query file/text
  user_query.FB.Phenotype <- reactive({
    if (input$FBPhenotypeQueryChoose == 'file') { 
      req(input$FB.Phenotype.query.file)
      ext <- tools::file_ext(input$FB.Phenotype.query.file$name)
      switch(ext,
             bed = fread(input$FB.Phenotype.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$FB.Phenotype.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with enhancer data
  FB.Phenotype.overlap <- eventReactive(input$FB.Phenotype.run, {
    req(input$FB.Phenotype.run)
      FB.Phenotype.query.overlap<- foverlaps(user_query.FB.Phenotype(), FB.Phenotype_table () , nomatch = 0) %>% 
        unique() %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)
      FB.Phenotype.query.overlap <- FB.Phenotype.query.overlap %>% 
        mutate(Overlap.start = FB.Phenotype.query.overlap[, ifelse(start > yourfile.start, start, yourfile.start)]) %>% 
        mutate(Overlap.end = FB.Phenotype.query.overlap[, ifelse(end < yourfile.end, end, yourfile.end)]) %>%
        mutate(Overlap.length = Overlap.end - Overlap.start)%>% unique() 
  })
  
  ## make variable for overlap table
  v_FB.Phenotype <- reactiveValues(table=NULL)
  
  observeEvent(input$FB.Phenotype.run, {
    req(input$FB.Phenotype.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      FB.Phenotype.overlap<-FB.Phenotype.overlap()%>% unique()
    })
    
    v_FB.Phenotype$table <- req(DT::datatable(FB.Phenotype.overlap (),escape = FALSE,options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
  },ignoreInit = TRUE)
  
  ## output
  output$FB.Phenotype_overlap_table <- renderDT({ v_FB.Phenotype$table }) 
  
  ## Download results
  shinyjs::disable("FB.Phenotype.download")
  observeEvent(FB.Phenotype.overlap(), {
    shinyjs::enable("FB.Phenotype.download")
  })
  
  output$FB.Phenotype.download <- downloadHandler(
    filename = "FB.Phenotype.download.csv",
    content = function(file) {
      write.csv(FB.Phenotype.overlap(), file, row.names = FALSE)
    }
  )
  
  ###  
  ### visualization panel sub-panel
  ### 
  
  ### Expression plot
  
  ## Read gene list

  ORDER <- data.frame(sample=c('Fetal_Heart.GSE63634' ,'Fetal_Heart','Fetal_Liver.GSE63634','Fetal_Liver','Fetal_Lung',
                               'Fetal_Skin.body','Fetal_Skeletal.Muscle','Fetal_Stomach','Fetal_Uterus','Fetal_Urinary',
                               'Fetal_Eye','Fetal_Frontal.Cortex','Fetal_Temporal.lobe','Fetal_Parietal.lobe',
                               'Fetal.Diencephalon.lobe','Fetal_Cerebellum','Fetal_Spinal.Cord','Fetal_Brain.GSE63634',
                               'Fetal_Brain.GSE82022','Fetal_Brain','Fetal_Brain.AllenBrain','Adult_Brain','Adult_Brain.AllenBrain'), order=c(1:23))
  FB.Expression.melt$order <- ORDER$order[match (FB.Expression.melt$variable, ORDER$sample)]
  
  ## make query
  FB.Expression.gene.query <- eventReactive(input$FB.Expression.plot, {
    unlist(strsplit(as.character(input$FB.Expression.Gene), '\n', fixed=TRUE))
  }, ignoreNULL= T)
  
  ## overlap between gene list and query value
  FB.Expression_plot<-reactive({ FB.Expression.melt %>% 
      subset (GeneName %in% c(toupper(FB.Expression.gene.query()))) %>% unique() 
  })
  
  ## Expression table
  #FB.Expression.gene.query <- eventReactive(input$FB.Expression.table, {
  #  unlist(strsplit(as.character(input$FB.Expression.Gene), '\n', fixed=TRUE))
  #}, ignoreNULL= T)
  
  FB_Expression_table <- reactive({
    data.frame(fread('Fetal_Brain/Expression_table.bed')) %>% 
      subset (GeneName %in% c(toupper(FB.Expression.gene.query()))) %>% unique()
  })
  
  ## make variable for query table and plot
  v_FB.expression <- reactiveValues(plot= NULL, table=NULL)
  
  observeEvent(input$FB.Expression.plot, {
    req(input$FB.Expression.plot)
    withProgress(message = 'Analysis in progress', value = 0, {
      if (input$FB.Expression.Choose.plot == "Heatmap plot") { 
        v_FB.expression$plot <- ggplot2::ggplot(FB.Expression_plot (), aes(x=variable, y=GeneName,fill=Log2_RPKM)) +
          geom_tile(color = "black") +
          scale_fill_gradient(low = "white", high = "red",name = "Log2 (Expression +1)") +
          coord_fixed()+
          theme(axis.text.x = element_text(size=12,color = "black",angle=90, hjust = 1,vjust=0.3),
                axis.text.y =element_text(size=12,color = "black"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank())
      } else if (input$FB.Expression.Choose.plot == "Line plot"){
        v_FB.expression$plot <- ggplot2::ggplot(FB.Expression_plot (), aes(x=variable, y=Log2_RPKM, group=GeneName, colour=GeneName)) +
          geom_line() + geom_point()+
          theme_classic()+ 
          scale_y_continuous(limits = c(0, NA),"Log2 (Expression +1)",expand = c(0, 0.15))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.3,color='Black', size=12),
                axis.text.y=element_text(size=12, color="black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=12, color="black"))
      } else v_FB.expression$plot <- ggplot2::ggplot(FB.Expression_plot (), aes(x=variable, y=Log2_RPKM, group=GeneName, fill=GeneName)) +
          geom_bar(position="dodge", stat="identity")+
          theme_classic()+ 
          scale_y_continuous("Log2 (Expression +1)",expand = c(0, 0.15))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.3,color='Black', size=12),
                axis.text.y=element_text(size=12, color="black"),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size=12, color="black"))
    })
    
  v_FB.expression$table <- req(DT::datatable(FB_Expression_table(),options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10),
                                              caption = htmltools::tags$caption ( htmltools::tags$span ("Expression level is based on RPKM/FPKM.", tags$br (), style = "color:black;"),
                                                                                  htmltools::tags$span (a(href="https://figshare.com/articles/dataset/SupplementaryTable_1/20533011","Click here, to know about expression data", tags$br (), style = "color:black;")),
                                                                                  style = 'caption-side: bottom;'),rownames = FALSE))
},ignoreInit = TRUE)


##  output
output$FB.Expression_plot <- renderPlot({ v_FB.expression$plot })
output$FB.Expression_table <- renderDT({ v_FB.expression$table })

  
  ## Download results
  shinyjs::disable("FB.Expression.download")
  observeEvent(FB_Expression_table(), {
    shinyjs::enable("FB.Expression.download")
  })
  
  output$FB.Expression.download <- downloadHandler(
    filename = "FB.Expression_Table.csv",
    content = function(file) {
      write.csv(FB_Expression_table(), file, row.names = FALSE)
    }
  )
  
  ###
  ### pLI
  ###
  
  ## pLI plot
  
  FB_pLI.gene.query <- eventReactive(input$FB.pLI.plot, {
    unlist(strsplit(as.character(input$FB.pLI.Gene), '\n', fixed=TRUE))
  }, ignoreNULL= T)
  
  FB_pLI_plot <- reactive({
    read.csv('External_Data/pLI.score.csv') %>% mutate(pLI=round(pLI,digits = 4)) %>% subset (gene %in% c(toupper(FB_pLI.gene.query()))) %>% unique()
  })
  
  v_FB_pLI <- reactiveValues(plot = NULL)
  
  observeEvent(input$FB.pLI.plot, {
    v_FB_pLI$plot <-ggplot2::ggplot(FB_pLI_plot (), aes(x=gene, y=pLI)) +
      geom_bar(stat="identity", position=position_dodge(),fill="darkgray")+
      theme_classic()+ 
      geom_text(aes(label=pLI), vjust= -1, color="red",position = position_dodge(0.8), size=3.5)+
      scale_y_continuous("pLI score", expand = c(0, 0),limits = c(0,1.1),breaks=seq(0, 1, by = 0.2)) +
      labs(title="Probability of loss-of-function intolerance (pLI)", x="", y="",
           caption ="
           pLI defines the probability of a gene being intolerant to variation causing loss of gene function.
           It is frequently used to prioritize candidate genes when analyzing whole exome or whole genome data. 
           It is ranged from 0 to 1 and in total, 3230 genes are identified as intolerant (pLI > 0.9) and 10374 as tolerant (pLI <0.1).
           
           pLI score is obtained from:           
           Lek M, et al. 2016. Exome Aggregation Consortium. Analysis of protein-coding genetic variation in 60,706 humans.PMID: 27535533.") +
      theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1,color='Black', size=12),
            axis.text.y=element_text(size=12, color="black"),
            plot.title = element_text(hjust = 0.5, size = 15,face = "bold"),
            plot.caption = element_text(hjust = 0,size=14,lineheight = 1.3))
  },ignoreInit = TRUE)
  
  
  ## output
  output$FB.pLI_plot <- renderPlot({ v_FB_pLI$plot })
  
  ## Download results
  shinyjs::disable("FB.pLI.download")
  observeEvent(FB_pLI_plot(), {
    shinyjs::enable("FB.pLI.download")
  })
  
  output$FB.pLI.download <- downloadHandler(
    filename = "pLI.Scores_Table.csv",
    content = function(file) {
      write.csv(FB_pLI_plot(), file, row.names = FALSE)
    }
  )
  
  ###
  ### DNA sequence features
  ###
  
  ## query file/text
  user_query.FB.DNA.features <- reactive({
    if (input$FBDNAfeaturesQueryChoose == 'file') { 
      req(input$FB.DNA.features.query.file)
      ext <- tools::file_ext(input$FB.DNA.features.query.file$name)
      switch(ext,
             bed = fread(input$FB.DNA.features.query.file$datapath,header=F)%>% 
               dplyr::rename (chr =V1, start=V2, end=V3) %>% setkey(chr, start, end)%>% unique()
      )
    }  else text <- data.table(read.csv(text=input$FB.DNA.features.query.text)) %>% setkey(chr, start, end)
  })
  ## overlap query file/text with PCR_DNA_features
  FB.DNA.features.overlap <- eventReactive(input$FB.DNA.features.run, {
    req(input$FB.DNA.features.run)
      FB.DNA.features.overlap <- foverlaps(user_query.FB.DNA.features(), FB_DNA.features.table , nomatch = 0) %>% 
        unique() %>% setkey(chr, start, end) %>% dplyr::rename (yourfile.start =i.start, yourfile.end=i.end)%>% unique()
  })
  
  ## make variable for overlap table and plot
  v_FB_DNA.features <- reactiveValues(plot = NULL,table=NULL)
  
  observeEvent(input$FB.DNA.features.run, {
    
    req(input$FB.DNA.features.run)
    withProgress(message = 'Analysis in progress', value = 0, {
      
      FB_DNA.features_overlap <-  FB.DNA.features.overlap () %>%dplyr::select(chr,start,end,GC.content,phastCons.score,nCER.score,LoF.tolerant.score) %>%
        reshape2 :: melt(id= c("chr","start","end"))%>% unique()%>% group_by(variable)%>%arrange (desc(value), .by_group = TRUE)%>% data.table()
    })
    
    FB_DNA.features_plot <- egg::ggarrange  ( ggplot2::ggplot(FB_DNA.features_overlap %>% subset(variable %in% "GC.content")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray28")+
                                                   theme_classic()+
                                                   scale_x_continuous("GC content", expand = c(0, 0)) +
                                                   labs(y="Enhancer")  ,
                                                 
                                                 ggplot2::ggplot(FB_DNA.features_overlap %>% subset(variable %in% "phastCons.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray47")+
                                                   theme_classic()+
                                                   scale_x_continuous("phastCons score", expand = c(0, 0)) +
                                                   labs(y="Enhancer"),
                                                 
                                                 ggplot2::ggplot(FB_DNA.features_overlap %>% subset(variable %in% "nCER.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray68")+
                                                   theme_classic()+
                                                   scale_x_continuous("ncER score", expand = c(0, 0)) +
                                                   labs(y="Enhancer"),
                                                 
                                                 ggplot2::ggplot(FB_DNA.features_overlap %>% subset(variable %in% "LoF.tolerant.score")%>% top_n(30), aes(x=value, y=paste0(chr,":", start,"-",end))) + 
                                                   geom_bar(position="dodge", stat="identity",fill="gray84")+
                                                   theme_classic()+
                                                   scale_x_continuous("LoF tolerant score", expand = c(0, 0)) +
                                                   labs(y="Enhancer") ,
                                                 
                                                 ncol = 4 ,nrow = 1)
    
    v_FB_DNA.features$plot <- ggpubr::annotate_figure(FB_DNA.features_plot, top = ggpubr::text_grob("Plots show only top 30 enriched regions", color = "red", face = "bold", size = 16))
    v_FB_DNA.features$table <- req(DT::datatable(FB.DNA.features.overlap(),options = list(orderClasses = TRUE,lengthMenu = c(5,10,25,50), pageLength = 10)))
    
  },ignoreInit = TRUE)
  
  
  ## output
  output$FB.DNA.features_plot <- renderPlot({ v_FB_DNA.features$plot }, height = 400, width = 1500)
  output$FB.DNA.features_table <- renderDT({ v_FB_DNA.features$table }) 
  
  
  ## Download results
  shinyjs::disable("FB.DNA.features.download")
  observeEvent(FB.DNA.features.overlap(), {
    shinyjs::enable("FB.DNA.features.download")
  })
  
  output$FB.DNA.features.download <- downloadHandler(
    filename = "DNA.feature_Scores_Table.csv",
    content = function(file) {
      write.csv(FB.DNA.features.overlap(), file, row.names = FALSE)
    }
  )
  
  ## UCSC FB sample Image 
  output$FB_UCSC_sample <- renderImage({
    list(src = "www/FB_UCSC.jpg",
         alt = "FB UCSC sample",
         width=870,height=800)
  }, deleteFile = F)
  
  ## FB enhancer paper Image
  output$FB.enhancer.paper_overview <- renderImage({
    list(src = "www/FB.Enhancer_overview.JPG",
         alt = "FB.enhancer.paper_overview",
         width=1000,height=500)
  }, deleteFile = F)
  
}



shinyApp(ui, server)









