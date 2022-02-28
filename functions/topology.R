# main functions ####
getStoredNetsChoices_topology_tab <- function() {
  snets <- StoredNets_topology_tab()
  if (nrow(snets) == 0)
    return(NULL)
  choices <- snets$id
  names(choices) <- snets$name
  return(choices)
}

getDatasetName <- function(id) {
  sn <- StoredNets()
  idi <- which(sn$id == id)
  if (is.null(sn) || nrow(sn) == 0 || length(idi) == 0)
    return(NULL)
  return(sn[idi,]$name)
}

fetchDataset_topology_tab <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}

fetchAllSelectedStoredDataset_topology_tab <- function() {
  ssn <- SelectedStoredNets_topology_tab()
  ids <- c()
  if (!is.null(ssn) && nrow(ssn) > 0) {
    ret <- list()
    for (i in 1:nrow(ssn)) {
      ret[[i]] <- fetchDataset_topology_tab(ssn[i,]$id)
      ids <- c(ids, ssn[i,]$id)
    }
    names(ret) <- ids
    return(ret)
  } else {
    return(NULL)
  }
}

# Statistics 
stat_dataset <- function(dataset, datasetName) {
  res <- tryCatch({
    columns <- c("Statistic", paste0("Value for ", datasetName))
    if (is.null(dataset)) {
      dataset <- EmptyDataset(columns)
    } else {
      igraph <- convert_to_igraph(dataset)
      if (length(input$statistics) == 0) {
        dataset <- EmptyDataset(columns)
      } else {
        dataset <- netstats(igraph, input$statistics)
        colnames(dataset) <- columns
      }
    }
    return(dataset)
  }, warning = function(w) {
    
  }, error = function(e) {
    cat(paste0("ERROR while executing stat_dataset: ", e, "\n"))
    return(NULL)
  }, finally = {
    
  })
  return(res)
}

stat_dataset2 <- function(dataset, datasetName) {
  res <- tryCatch({
    columns <- c("Statistic", paste0("Value for ", datasetName))
    if (is.null(dataset)) {
      dataset <- EmptyDataset(columns)
    } else {
      igraph <- convert_to_igraph(dataset)
      if (length(input$statistics2) == 0) {
        dataset <- EmptyDataset(columns)
      } else {
        dataset <- netstats(igraph, input$statistics2)
        colnames(dataset) <- columns
      }
    }
    return(dataset)
  }, warning = function(w) {
    
  }, error = function(e) {
    cat(paste0("ERROR while executing stat_dataset: ", e, "\n"))
    return(NULL)
  }, finally = {
    
  })
  return(res)
}

# sub-routines ####
SelectedStoredNets_topology_tab <- function() {
  if (length(reactiveVars$SelectedStoredNetworksIds_topology_tab) > 0) {
    return(StoredNets_topology_tab()[which(
      reactiveVars$StoredNetworks_topology_tab$id %in%
        reactiveVars$SelectedStoredNetworksIds_topology_tab
    ),])
  }
  else if (nrow(StoredNets_topology_tab()) == 0 ||
           is.na(StoredNets_topology_tab()[1,]))
    return(NULL)
  else {
    updateCheckboxGroupInput(
      session,
      "uiStoredGraphsOutputSelectTopolopgy",
      "Selected network(s)",
      choices = getStoredNetsChoices_topology_tab(),
      selected = getStoredNetsChoices_topology_tab()[1]
    )
    return(StoredNets_topology_tab()[1,])
  }
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
