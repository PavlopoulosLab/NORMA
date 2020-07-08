### Dependencies ###
if (!require(Rcpp))
  install.packages("Rcpp")
if (!require(shiny))
  install.packages('shiny')
# make sure you load DT *after* shiny
if (!require(DT))
  install.packages('DT')
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
#BiocManager::install()
#source("https://bioconductor.org/biocLite.R")
if (!require(uuid))
  install.packages('uuid')
if (!require(shinyBS))
  install.packages('shinyBS')
if (!require(networkD3))
  install.packages('networkD3')
if (!require(shinyjs))
  install.packages('shinyjs')
if (!require(magrittr))
  install.packages('magrittr')
if (!require(devtools))
  install.packages('devtools')
if (!require(colourpicker))
  install.packages('colourpicker')
if (!require(backports))
  install.packages('backports')

if (!require(viridis))
  install.packages('viridis')
if (!require(tidyverse))
  install.packages('tidyverse')
if (!require(plyr))
  install.packages('plyr')
if (!require(dplyr))
  install.packages('dplyr')
if (!require(purrr))
  install.packages('purrr')
if (!require(igraph))
  install.packages('igraph')
if (!require(RColorBrewer))
  install.packages('RColorBrewer')
if (!require(data.table))
  install.packages('data.table')
if (!require(stringi))
  install.packages('stringi')

if (!require(shinythemes))
  install.packages('shinythemes')
if (!require(rhandsontable))
  install.packages('rhandsontable')

if (!require(d3Network)) install.packages("d3Network")
if (!require(lattice)) install.packages("lattice")
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
if (!require(networkD3)) install.packages("networkD3")
if (!require(randomcoloR)) install.packages("randomcoloR")
if (!require(Cairo)) install.packages("Cairo")
if (!require(plotly)) install.packages("Plotly")



library(shinythemes)

library(BiocManager)
library(devtools)
library(shiny)
library(shinyBS)
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

options(shiny.usecairo = F)
options(shiny.maxRequestSize=30*1024^2) #30 MB for uploaded networks
options(shiny.reactlog=TRUE) #debugging

source("layout_choices.R")
source("layouts_ui.R")
source("statistics.R")
source("netstats.R")
source("automated_annotations_vector.R")
source("automated_annotations_choices.R")
source("layouts_3D.R")
source("layout_choices_3D.R")


# Images for help pages and banner
b64_1 <-
  base64enc::dataURI(file = "Figures/Banner.png", mime = "image/png")
b64_2 <-
  base64enc::dataURI(file = "Figures/Upload-1.png", mime = "image/png")
b64_3 <-
  base64enc::dataURI(file = "Figures/Upload-2.png", mime = "image/png")
b64_4 <-
  base64enc::dataURI(file = "Figures/Upload-3.png", mime = "image/png")
b64_5 <-
  base64enc::dataURI(file = "Figures/Network_view_interactive.png", mime =
                       "image/png")
b64_6 <-
  base64enc::dataURI(file = "Figures/Community_detection_algorithms.png", mime =
                       "image/png")
b64_7 <-
  base64enc::dataURI(file = "Figures/Convex_Hulls.PNG", mime = "image/png")
b64_8 <-
  base64enc::dataURI(file = "Figures/Pies.PNG", mime = "image/png")
b64_9 <-
  base64enc::dataURI(file = "Figures/Expression_colors.png", mime = "image/png")
b64_10 <-
  base64enc::dataURI(file = "Figures/Topology.PNG", mime = "image/png")
b64_11 <-
  base64enc::dataURI(file = "Figures/Topology_comparisons.PNG", mime = "image/png")
b64_12 <-
  base64enc::dataURI(file = "Figures/venn.png", mime = "image/png")
b64_3D_convex <- base64enc::dataURI(file = "Figures/Convex_Hulls_3D.PNG", mime = "image/png")


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


# Colors if there is NOT "NA" in dataset (nodes)
group_pal_rows<- function(n){
  
  qual_col_pals[1:300]
    
  if(n>=300){
    qual_col_pals<-c(qual_col_pals, rep(c("grey50"), times = (n-300) ))
    nn<-  qual_col_pals[1:n]
    nnames<-c(nn)
  }
  else
  {
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
  else
  {
    nn<-  qual_col_pals[1:n]
    nnames<-c(nn,"white")
  }
}


# DataTable / Legend colors
css_colors <- group_pal_rows(300)

css_generated <- ""
for (i in 1: 300)
{
  css_generated<-paste(css_generated, ".x", i, "{background-color: ", css_colors[i],";}","table.dataTable tr.selected td.x",i,"{background-color: ",css_colors[i], " !important;}",sep="")
  css_generated<-paste(css_generated," ", sep="\n")
}


max_pixels_panel<-5000
mapper<-function(value, istart, istop, ostart, ostop)
{ 
  return (ostart + (ostop - ostart) * ((value - istart) / (istop - istart)))
}


  
