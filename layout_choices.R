layout_choices <- function(igraph,layouts_ui){
  if(length(layouts_ui)==0)
    return(NULL)
  results<-list()
  for(i in layouts_ui){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    # print(paste0("Calculating layouts_ui: ",command))
    results[[description]]<-eval(parse(text=command))
    # print(results[[description]])
  }
  # coords_results <- results[[description]]
  # print(coords_results)
  # 
  # graph <- get.edgelist(igraph)
  # a<- data.frame(V1= n[,1])
  # b<- data.frame(V1= n[,2])
  # m<- full_join(a,b)
  # 
  # names <- unique(m)
  # hash_coords <- new.env(hash = TRUE)
  # for (i in 1:length(V(igraph))) {
  #   hash_coords[[ as.character(names[i,1]) ]] <- c(coords_results[[i,1]],coords_results[[i,2]])
  # }
  
  # return(hash_coords)
  return(results[[description]])
}




