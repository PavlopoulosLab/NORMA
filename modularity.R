modularity<- function(method){

  set.seed(123)
  
  clp <- automated_annotation_choices(net, automated_annotations)
  
groups_all<- c()
  for (i in 1:length(clp)){
    clp_i <- as.data.frame(clp[[i]])
    ss<-paste(clp_i[,1], collapse="," )
    groups_all[[i]]<-ss
  }

prefix <- "Group-"
suffix <- seq(1:length(clp))

max.length <- max(sapply(groups_all, length))
l <- lapply(groups_all, function(v) { c(v, rep(NA, max.length-length(v)))})
df<-as.data.frame(do.call(rbind, l))
groups_column_name<- paste(prefix, suffix, sep="")
df<- cbind(groups_column_name, df)
names(df) <- NULL

# print(df)
# write.table(df, file="tmp.txt", row.names = F, col.names = F,sep = "\t")
# 
# annoation_graph<- read.delim("tmp.txt", header = F) 
# print(annoation_graph)

return(df)
}

