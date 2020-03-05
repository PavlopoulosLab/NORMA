netstats <- function(igraph,statistics){
	#   prepare<-as.data.frame(gsub("igraph","testing",statistics))
	if(length(statistics)==0)
		return(NULL)
	results<-list()
	for(i in statistics){
		tmp<-unlist(strsplit(i,"\t",fixed=T))
		description<-tmp[1]
		command<-tmp[2]
		# print(paste0("Calculating statistic: ",command))
		results[[description]]<-eval(parse(text=command))
	}
	return(data.frame(cbind(names(results),as.character(results))))
}



