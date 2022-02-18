# main functions ####
getStoredNetsChoices_just_network <- function() {
  snets <- StoredNets_just_network()
  if (nrow(snets) == 0) return(NULL)
  choices <- snets$id
  names(choices) <- snets$name
  return(choices)
}

fetchFirstSelectedStoredIgraph_just_network <- function() {
  dataset <- fetchFirstSelectedStoredDataset_just_network()
  if (is.null(dataset))
    return(NULL)
  else
    return(convert_to_igraph(dataset))
}

# Download-button for automated annotations
file_name <- function() {
  automated_annotations <- input$automated_annotations
  
  if (automated_annotations == "Fast-Greedy\tcluster_fast_greedy(igraph)") {
    filename <- "Fast-Greedy_"
  }
  if (automated_annotations == "Louvain\tcluster_louvain(igraph)") {
    filename <- "Louvain_"
  }
  if (automated_annotations == "Label-Propagation\tcluster_label_prop(igraph)") {
    filename <- "Label-Propagation_"
  }
  if (automated_annotations == "Walktrap\tcluster_walktrap(igraph)") {
    filename <- "Walktrap_"
  }
  if (automated_annotations == "Betweenness\tcluster_edge_betweenness(igraph)") {
    filename <- "Betweenness_"
  }
  return(filename)
}

# sub-routines ####
fetchFirstSelectedStoredDataset_just_network <- reactive({
  ssn <- SelectedStoredNets_just_network()
  if (!is.null(ssn) && nrow(ssn) > 0) {
    return(fetchDataset_just_network(ssn[1,]$id))
  } else {
    return(NULL)
  }
})

StoredNets_just_network <- reactive({
  return(reactiveVars$StoredNetworks_just_network)
})

SelectedStoredNets_just_network <- function() {
  if (length(reactiveVars$SelectedStoredNetworksIds_just_network) > 0) {
    return(StoredNets_just_network()[which(
      reactiveVars$StoredNetworks_just_network$id %in%
        reactiveVars$SelectedStoredNetworksIds_just_network
    ),])
  }
  else if (nrow(StoredNets_just_network()) == 0 ||
           is.na(StoredNets_just_network()[1,]))
    return(NULL)
  else {
    updateCheckboxGroupInput(
      session,
      "uiLoadGraphOptionsOutput_just_network",
      "Selected network(s)",
      choices = getStoredNetsChoices_just_network(),
      selected = getStoredNetsChoices_just_network()[1]
    )
    return(StoredNets_just_network()[1,])
  }
}

fetchDataset_just_network <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}

convert_to_igraph <- function(dataset1){
  weighted_tf <- FALSE
  if (attr(dataset1, "weighted")) {
    weighted_tf <- attr(dataset1, "weighted")
  }
  set.seed(123)
  igraph <- graph.data.frame(dataset1, directed = F, vertices = NULL)
  if (attr(dataset1, which = "weighted"))
    E(igraph)$weight <- dataset1$Weight
  return(igraph)
}

