layout_choices_3D <- function(igraph, layouts_3D){
  if(length(layouts_3D)==0)
    return(NULL)
  results<-list()
  for(i in layouts_3D){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    # print(paste0("Calculating layouts_3D: ",command))
    results[[description]]<-eval(parse(text=command))
    # print(results[[description]])
  }
  
  return(results[[description]])
}


