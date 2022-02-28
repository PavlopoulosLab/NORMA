if (!require(tidyverse))
  install.packages('tidyverse')
if (!require(plyr))
  install.packages('plyr')
if (!require(dplyr))
  install.packages('dplyr')
if (!require(purrr))
  install.packages('purrr')
if (!require(stringi))
  install.packages('stringi')
if (!require(stringr))
  install.packages('stringr')

library(stringi)
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(purrr)

network<- read.delim(choose.files())
annotations <- read.delim(choose.files(),header = F)

column1<- unique(network$Source)
column2<- unique(network$Target)

df1<- data.frame(V1= column1)
df2<- data.frame(V1= column2)

unique_nodes_network <- suppressMessages(full_join(df1, df2))

annotations2<- as.character(annotations[,2])
genes <- strsplit(annotations2, ',')

unique_nodes_annotations<- as.data.frame(unique(unlist(genes)))
colnames(unique_nodes_annotations)<- 'V1'

merged <- suppressMessages(full_join(unique_nodes_network, unique_nodes_annotations))


words_to_be_removed <- suppressMessages(anti_join(merged,unique_nodes_network))

annotations_new<- data.frame()
if(!is.null(words_to_be_removed)){
  for(i in 1:length(annotations[,2])){
    x <- annotations[i,2]
    candidates <- strsplit(as.character(x), ',')
    line<-''
     for(j in 1:length(candidates[[1]])){
        word<-as.character(c(candidates[[1]])[j])
        if(!(word %in% words_to_be_removed$V1))
          line<-paste(line,word, sep=',')
     }
    line <- gsub('^,', '', line)
    if(nchar(line)>0)
    {
      annotations_new[i,1] <- annotations[i,1]
      annotations_new[i,2] <- line
    }
  }
  annotations_new<-na.omit(annotations_new)
} else {
  annotations_new<-annotations
}
colnames(annotations_new)<- c('Annotations','Nodes')

write.table(annotations_new, file = 'annotations_cleaned.txt', row.names = F, col.names = F, sep = "\t")

