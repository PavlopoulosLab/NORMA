# Install dependencies ####
if (!require(Rcpp)) install.packages("Rcpp")
if (!require(shiny)) install.packages('shiny')
if (!require(DT)) install.packages('DT')
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!require(uuid))install.packages('uuid')
if (!require(shinyBS)) install.packages('shinyBS')
if (!require(networkD3)) install.packages('networkD3')
if (!require(shinyjs)) install.packages('shinyjs')
if (!require(shinyalert)) install.packages('shinyalert')
if (!require(magrittr)) install.packages('magrittr')
if (!require(devtools)) install.packages('devtools')
if (!require(colourpicker)) install.packages('colourpicker')
if (!require(backports)) install.packages('backports')
if (!require(viridis)) install.packages('viridis')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(plyr)) install.packages('plyr')
if (!require(dplyr)) install.packages('dplyr')
if (!require(purrr)) install.packages('purrr')
if (!require(igraph)) install.packages('igraph')
if (!require(RColorBrewer)) install.packages('RColorBrewer')
if (!require(data.table)) install.packages('data.table')
if (!require(stringi)) install.packages('stringi')
if (!require(shinythemes)) install.packages('shinythemes')
if (!require(rhandsontable)) install.packages('rhandsontable')
if (!require(d3Network)) install.packages("d3Network")
if (!require(lattice)) install.packages("lattice")
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if (!require(networkD3)) install.packages("networkD3")
if (!require(randomcoloR)) install.packages("randomcoloR")
if (!require(Cairo)) install.packages("Cairo")
if (!require(plotly)) install.packages("Plotly")

# Load libraries ####
library(shinythemes)
library(BiocManager)
library(devtools)
library(shiny)
library(shinyBS)
library(shinyalert)
library(DT)
library(uuid)
library(networkD3)
library(magrittr)
library(shinyjs)
library(colourpicker)
library(shinyWidgets)
library(plyr)
library(tidyr)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(RColorBrewer)
library(data.table)
library(networkD3)
library(stringi)
library(reshape2)
library(shinyjs)
library(visNetwork)
library(shinyWidgets)
library(jsonlite)
library(d3Network)
library(lattice)
library(Cairo)
library(randomcoloR)
library(colourpicker)
library(VennDiagram)
library(reactlog) #debugging

# Options ####
options(shiny.usecairo = F)
options(shiny.maxRequestSize = 30*1024^2) # 30 MB for uploaded networks
options(shiny.reactlog = TRUE) # debugging
options(shiny.error = browser) # debugging

# Global variables ####
ui_options <- c(ui_table_line_height = "80%", ui_table_font_sz = "80%")
layouts_ui <- c(
  "Fruchterman-Reingold"="Fructerman\tlayout_nicely(igraph, dim=2)",
  # "Fruchterman-Reingold"="Fructerman\tlayout.fruchterman.reingold(igraph, dim=2)",
  "Random"="Random\tlayout.random(igraph, dim=2)",
  "Circle"="Circle\tlayout.circle(igraph)",
  "Kamada-Kawai"="Kamada-Kawai\tlayout.kamada.kawai(igraph, dim=2)",
  "Reingold-Tilford"="Reingold-Tilford\tlayout.reingold.tilford(igraph)",
  "Lgl"="Lgl\tlayout.lgl(igraph)",
  "Graphopt"="Graphopt\tlayout.graphopt(igraph)",
  "Gem"="Graphopt\tlayout.gem(igraph)",
  "Star"="Graphopt\tlayout_as_star(igraph)",
  "Grid"="Grid\tlayout.grid(igraph)"
  # "Auto"="Auto\tlayout.auto(igraph, dim=2)",
  # "SVD"="SVD\tlayout.svd(igraph, d=shortest.paths(igraph))"
  # "Nicely"="Nicely\tlayout_nicely(igraph, dim=2)",
  # "Tree"="Graphopt\tlayout_as_tree(igraph)",
  # "Spring"="Spring\tlayout.spring(igraph)",
  # "Fruchterman-Reingold Grid"="Fruchterman Grid\tlayout.fruchterman.reingold.grid(igraph)",
  # "Sphere"="Sphere\tlayout.sphere(igraph)",
)
selected_layouts <- c( "Fructerman\tlayout_nicely(igraph, dim=2)" )
layouts_3D<-c(
  "Fruchterman-Reingold"="Fructerman\tlayout_with_fr(igraph, dim=3)",
  "Random"="Random\tlayout.random(igraph, dim=3)",
  "Kamada-Kawai"="Kamada-Kawai\tlayout.kamada.kawai(igraph, dim=3)",
  # "Reingold-Tilford"="Reingold-Tilford\tlayout.reingold.tilford(igraph, dim=3)",
  # "Lgl"="Lgl\tlayout.lgl(igraph, dim=3)",
  # "Graphopt"="Graphopt\tlayout.graphopt(igraph, dim=3)",
  # "Gem"="Graphopt\tlayout.gem(igraph, dim=3)",
  "Grid"="Grid\tlayout.grid(igraph, dim=3)"
)
selected_layouts_3d <- c( "Fructerman\tlayout_with_fr(igraph, dim=3)" )
automated_annotations_ui <- c(
  "Fast-Greedy"="Fast-Greedy\tcluster_fast_greedy(igraph)",
  "Louvain"="Louvain\tcluster_louvain(igraph)",
  "Label-Propagation"="Label-Propagation\tcluster_label_prop(igraph)",
  "Walktrap"="Walktrap\tcluster_walktrap(igraph)",
  "Betweenness"="Betweenness\tcluster_edge_betweenness(igraph)"
)
selected_automated_annotations <- c( "Fast-Greedy\tcluster_fast_greedy(igraph)" )
statistics <- c(
  "Number of Edges" = "Number of Edges\tecount(igraph)",
  "Number of Nodes" = "Number of Nodes\tvcount(igraph)",
  "Density" = "Density\tgraph.density(igraph)",
  "Average path length" = "Average path length\taverage.path.length(igraph)",
  "Clustering Coefficient" = "Clustering Coefficient\ttransitivity(igraph)",
  "Modularity" = "Modularity\tmodularity(igraph,membership(walktrap.community(igraph)))",
  "Average Eccentricity" = "Average Eccentricity\tmean(eccentricity(igraph))",
  "Average number of Neighbors" = "Average number of Neighbors\t(centr_eigen(igraph)$centralization)",
  "Centralization betweenness" = "Centralization.betweenness\tcentralization.betweenness(igraph)$centralization",
  # "Centralization closeness" = "Centralization.closeness\tcentralization.closeness(igraph)$centralization",
  "Centralization degree" = "Centralization.degree\tcentralization.degree(igraph)$centralization"
)
selected_statistics <- c("Number of Edges" = "Number of Edges\tecount(igraph)")

# 300 Colors - Up to 100 are distinct
qual_col_pals<-c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666","#7FC97F","#BEAED4",
                 "#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99",
                 "#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#a17bc9","#B15928","#FBB4AE","#B3CDE3","#CCEBC5","#a9a9a9",
                 "#dcdcdc","#98fb98","#556b2f","#8b4513","#6b8e23","#2e8b57","#800000","#FDDAEC","#006400","#808000",
                 "#6c3920","#778899","#3cb371","#bc8f8f","#b0737c","#008080","#b8860b","#bdb76b","#cd853f","#4682b4",
                 "#d2691e","#9acd32","#20b2aa","#008b7b","#82007e","#32cd32","#8fbc8f","#0ba47e","#b03060","#d2b48c",
                 "#66cdaa","#9932cc","#ff0000","#ffa500","#ffd700","#ffff00","#c71585","#0303ff","#7fff00","#00ff00",
                 "#ba55d3","#00ff7f","#4169e1","#e9967a","#dc143c","#00ffff","#00bfff","#9370db","#0000ff","#a020f0",
                 "#adff2f","#d8bfd8","#ff7f50","#ff00ff","#db7093","#f0e68c","#fa8072","#ffff54","#6495ed","#dda0dd",
                 "#87ceeb","#ff1493","#afeeee","#ee82ee","#2f4f4f","#7fffd4","#ff69b4","#ffe4c4","#ffb6c1","#DECBE4",
                 "#FED9A6","#FFFFCC","#E5D8BD","#191970","#F2F2F2","#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9",
                 "#FFF2AE","#F1E2CC","#CCCCCC","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FFFF33","#A65628","#F781BF",
                 "#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3","#8DD3C7",
                 "#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#095F02",
                 "#E31A97","#A81AD2","#74ED33","#38F76B","#E607E1","#17D214","#E2E749","#5522DF","#1FF696","#1406E9",
                 "#6A43DE","#C2F309","#E92D57","#5004DE","#9C4AE9","#2EF613","#EDAA2D","#2FB5EA","#47FAB2","#CC9221",
                 "#5C4AFC","#E843D0","#FE3F64","#21DD3A","#1988D2","#BC11EF","#EEFB3D","#1B9E77","#D95F02","#7570B3",
                 "#88e99a","#ca2dc5","#bce333","#643176","#34f50e","#b22839","#4be8f9","#6c3920","#bfcd8e","#1642cd",
                 "#f4d403","#5310f0","#609111","#c697f4","#34466d","#b3d9fa","#155126","#fab5b5","#0ba47e","#ff0087",
                 "#3d99ce","#bf711e","#fa718e","#798872","#fe5900","#b0737c","#E31A97","#A81AD2","#74ED33","#38F76B",
                 "#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC","#F2F2F2","#B3E2CD","#FDCDAC","#CBD5E8","#F4CAE4",
                 "#E6F5C9","#FFF2AE","#F1E2CC","#CCCCCC","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FFFF33","#A65628",
                 "#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3",
                 "#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD",
                 "#095F02","#E31A97","#A81AD2","#74ED33","#38F76B","#E607E1","#17D214","#E2E749","#5522DF","#1FF696",
                 "#1406E9","#6A43DE","#C2F309","#E92D57","#5004DE","#9C4AE9","#2EF613","#EDAA2D","#2FB5EA","#47FAB2",
                 "#CC9221","#5C4AFC","#E843D0","#FE3F64","#21DD3A","#1988D2","#BC11EF","#EEFB3D","#1B9E77","#D95F02",
                 "#7570B3","#88e99a","#ca2dc5","#bce333","#643176","#34f50e","#b22839","#4be8f9","#483d8b","#bfcd8e",
                 "#1642cd","#f4d403","#5310f0","#609111","#c697f4","#34466d","#b3d9fa","#155126","#fab5b5","#800080",
                 "#ff0087","#3d99ce","#bf711e","#fa718e","#798872","#fe5900","#663399","#E31A97","#A81AD2","#74ED33")

# Images for help pages and banner
b64_1 <- base64enc::dataURI(file = "./www/Figures/Banner.png", mime = "image/png")
b64_2 <- base64enc::dataURI(file = "./www/Figures/Upload-1.png", mime = "image/png")
b64_3 <- base64enc::dataURI(file = "./www/Figures/Upload-2.png", mime = "image/png")
b64_4 <- base64enc::dataURI(file = "./www/Figures/Upload-3.png", mime = "image/png")
b64_5 <- base64enc::dataURI(file = "./www/Figures/Network_view_interactive.png", mime = "image/png")
b64_6 <- base64enc::dataURI(file = "./www/Figures/Community_detection_algorithms.png", mime = "image/png")
b64_7 <- base64enc::dataURI(file = "./www/Figures/Convex_Hulls.PNG", mime = "image/png")
b64_8 <- base64enc::dataURI(file = "./www/Figures/Pies.PNG", mime = "image/png")
b64_9 <- base64enc::dataURI(file = "./www/Figures/Expression_colors.png", mime = "image/png")
b64_10 <- base64enc::dataURI(file = "./www/Figures/Topology.PNG", mime = "image/png")
b64_11 <- base64enc::dataURI(file = "./www/Figures/Topology_comparisons.PNG", mime = "image/png")
b64_12 <- base64enc::dataURI(file = "./www/Figures/venn.png", mime = "image/png")
b64_3D_convex <- base64enc::dataURI(file = "./www/Figures/Convex_Hulls_3D.PNG", mime = "image/png")

# Help pages examples for download
dros_net <- read.delim("./www/Examples/TAU/TAU_network_DEGs_NORMA.txt", header = T)
dros_annot <- read.delim("./www/Examples/TAU/TAU_KEGG_Annotation_NORMA.txt", header = F)
dros_louvain <- read.delim("./www/Examples/TAU/TAU_Louvain.txt", header = F)
dros_express <- read.delim("./www/Examples/TAU/TAU_expressions.txt", header = F)
string_net_tp53 <- read.delim("./www/Examples/TP53/string_interactions.txt", header = T)
string_annot <- read.delim("./www/Examples/TP53/string_interactions_groups_comma_duplicate.txt", header = F)
string_expr <- read.delim("./www/Examples/TP53/string_expression_colors.txt", header = F)
string_net_bcar3 <- read.delim("./www/Examples/BCAR3/BCAR3.txt", header = T)
string_bp <- read.delim("./www/Examples/BCAR3/BCAR3_GO_BP.txt", header = F)
string_mf <- read.delim("./www/Examples/BCAR3/BCAR3_GO_MF.txt", header = F)
string_kegg <- read.delim("./www/Examples/BCAR3/BCAR3_KEGG.txt", header = F)
co_express <- read.delim("./www/Examples/Human_Coexpression/NORMA_Human_coexpression_NETWORK.txt", header = T)
co_express_bp <- read.delim("./www/Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_GO_BP.txt", header = F)
co_express_mf <- read.delim("./www/Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_GO_MF.txt", header = F)
co_express_cc <- read.delim("./www/Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_GO_CC.txt", header = F)
co_express_kegg <- read.delim("./www/Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_KEGG.txt", header = F)
co_express_mcode <- read.delim("./www/Examples/Human_Coexpression/NORMA_Human_coexpression_Expression_MCODE.txt", header = F)
covid_19_net <- read.delim("./www/Examples/IntAct_COVID19/Intact-data_COVID19_no_self_loops.txt", header = T)
covid_19_interpro <- read.delim("./www/Examples/IntAct_COVID19/HomoSapiens_Protein_Domains_INTERPRO_FILTERED.txt", header = F)
covid_19_bp <- read.delim("./www/Examples/IntAct_COVID19/HomoSapiens_Gene_Ontology_GOTERM_BP_DIRECT_FILTERED.txt", header = F)
covid_19_mf <- read.delim("./www/Examples/IntAct_COVID19/HomoSapiens_Gene_Ontology_GOTERM_MF_DIRECT_FILTERED.txt", header = F)
covid_19_cc <- read.delim("./www/Examples/IntAct_COVID19/HomoSapiens_Gene_Ontology_GOTERM_CC_DIRECT_FILTERED.txt", header = F)
covid_19_kegg <- read.delim("./www/Examples/IntAct_COVID19/HomoSapiens_Pathways_KEGG_PATHWAY_FILTERED.txt", header = F)
covid_19_smart <- read.delim("./www/Examples/IntAct_COVID19/HomoSapiens_Protein_Domains_SMART_FILTERED.txt", header = F)
Gallus_gallus_net <- read.delim("./www/Examples/BioGrid_Chicken_Gallus/Biogrid_no_self_loops.txt", header = T)
Gallus_gallus_kegg <- read.delim("./www/Examples/BioGrid_Chicken_Gallus/BioGrid_Chicken_Gallus_Pathways_KEGG_PATHWAY_FILTERED.txt", header = F)

R_script <- read.delim("./www/annotation_cleaner.R", header = F)

# General functions ####
# functions for selectbox choices
layout_choices <- function(igraph,layouts_ui){
  if(length(layouts_ui)==0)
    return(NULL)
  results<-list()
  for(i in layouts_ui){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]]<-eval(parse(text=command))
  }
  return(results[[description]])
}

layout_choices_3D <- function(igraph, layouts_3D){
  if(length(layouts_3D)==0)
    return(NULL)
  results<-list()
  for(i in layouts_3D){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]]<-eval(parse(text=command))
  }
  
  return(results[[description]])
}

automated_annotation_choices <- function(igraph,automated_annotations_ui){
  if(length(automated_annotations_ui)==0) return(NULL)
  results<-list()
  
  for(i in automated_annotations_ui){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]] <- eval(parse(text=command))
  }
  return(results[[description]])
}

netstats <- function(igraph,statistics){
  if(length(statistics)==0)
    return(NULL)
  results<-list()
  for(i in statistics){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]]<-eval(parse(text=command))
  }
  return(data.frame(cbind(names(results),as.character(results))))
}

# Colors if there is NOT "NA" in dataset (nodes)
group_pal_rows<- function(n){
  
  qual_col_pals[1:300]
    
  if(n>=300){
    qual_col_pals<-c(qual_col_pals, rep(c("grey50"), times = (n-300) ))
    nn<-  qual_col_pals[1:n]
    nnames<-c(nn)
  }
  else {
    nn<-  qual_col_pals[1:n]
    nnames<-c(nn)
  }
}

# Colors if there is "NA" in dataset (nodes)
group_palette<- function(n){
  qual_col_pals[1:300]
  
  if(n>=300){
    qual_col_pals<-c(qual_col_pals, rep(c("grey50"), times = (n-300) ))
    nn<-  qual_col_pals[1:n]
    nnames<-c(nn,"white")
  }
  else {
    nn<-  qual_col_pals[1:n]
    nnames<-c(nn,"white")
  }
}

# DataTable / Legend colors
css_colors <- group_pal_rows(300)

css_generated <- ""
for (i in 1: 300){
  css_generated<-paste(css_generated, ".x", i, "{background-color: ", css_colors[i],";}","table.dataTable tr.selected td.x",i,"{background-color: ",css_colors[i], " !important;}",sep="")
  css_generated<-paste(css_generated," ", sep="\n")
}

max_pixels_panel<-5000
mapper<-function(value, istart, istop, ostart, ostop){ 
  return (ostart + (ostop - ostart) * ((value - istart) / (istop - istart)))
}

# ui helper function
ui_dataTable_panel <- function(datasetName, pagination = TRUE) {
  return(parse(
    text = paste0(
      "div(div(DT::dataTableOutput('",
      datasetName,
      "'), class='box-panel-padding'), class='box-panel')"
    )
  ))
}

read_expressions <-
  function(datapath,
           type = c("txt"),
           header = F,
           sep = "\t",
           quote = "\"",
           weighted = F,
           na.strings = c("", "NA"))
    ({
      expression1 <-
        read.table(datapath,
                   header = header,
                   sep = sep,
                   quote = quote,
                   comment.char="?")
    })


EmptyDataset <- function(columns) {
  dataset <- data.frame(V1 = integer())
  lapply(columns[-1], function(x)
    dataset[, x] <<- integer())
  colnames(dataset) <- columns
  return(dataset)
}
