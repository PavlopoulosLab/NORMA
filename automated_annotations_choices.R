automated_annotation_choices <- function(igraph,automated_annotations_ui){
  if(length(automated_annotations_ui)==0)
    return(NULL)
  results<-list()
  for(i in automated_annotations_ui){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]]<-eval(parse(text=command))
  }
  return(results[[description]])
}
