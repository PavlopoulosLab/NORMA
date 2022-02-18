# main functions ####
getStoredNetsChoices_annotations_tab <- function() {
  snets <- StoredNets_annotations_tab()
  if (nrow(snets) == 0) return(NULL)
  choices <- snets$id
  names(choices) <- snets$name
  return(choices)
}

getStoredNetsChoices2_annotations_tab <- function() {
  snets <- StoredNets2_annotations_tab()
  if (nrow(snets) == 0)
    return(NULL)
  choices <- snets$id
  names(choices) <- snets$name
  return(choices)
}

getStoredExpressionChoices <- function() {
  sexpress <- StoredExpress()
  if (nrow(sexpress) == 0)
    return(NULL)
  choices <- sexpress$id
  names(choices) <- sexpress$name
  return(choices)
}

fetchFirstSelectedStoredIgraph_annotations_tab <- function() {
  dataset <- fetchFirstSelectedStoredDataset_annotations_tab()
  if (is.null(dataset))
    return(NULL)
  else
    return(convert_to_igraph(dataset))
}

fetchDataset_annotations_tab <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}

SelectedStoredNets_annotations_tab <- function() {
  if (length(reactiveVars$SelectedStoredNetworksIds_annotations_tab) > 0) {
    return(StoredNets_annotations_tab()[which(
      reactiveVars$StoredNetworks_annotations_tab$id %in%
        reactiveVars$SelectedStoredNetworksIds_annotations_tab
    ),])
  }
  else if (nrow(StoredNets_annotations_tab()) == 0 ||
           is.na(StoredNets_annotations_tab()[1,]))
    return(NULL)
  else {
    updateCheckboxGroupInput(
      session,
      "uiLoadGraphOptionsOutput_annotations_tab",
      "Selected network(s)",
      choices = getStoredNetsChoices_annotations_tab(),
      selected = getStoredNetsChoices_annotations_tab()[1]
    )
    return(StoredNets_annotations_tab()[1,])
  }
}

fetchDataset2_annotations_tab <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}

SelectedStoredNets2_annotations_tab <- function() {
  if (length(reactiveVars$SelectedStoredNetworksIds2_annotations_tab) > 0) {
    return(StoredNets2_annotations_tab()[which(
      reactiveVars$StoredNetworks2_annotations_tab$id %in%
        reactiveVars$SelectedStoredNetworksIds2_annotations_tab
    ),])
  }
  else if (nrow(StoredNets2_annotations_tab()) == 0 ||
           is.na(StoredNets2_annotations_tab()[1,]))
    return(NULL)
  else {
    updateCheckboxGroupInput(
      session,
      "uiLoadGraphOptionsOutput_annotations_annotations_tab",
      "Selected network(s)",
      choices = getStoredNetsChoices2_annotations_tab(),
      selected = getStoredNetsChoices2_annotations_tab()[1]
    )
    return(StoredNets2_annotations_tab()[1,])
  }
}

### Output after choosing selected annotations ###
SelectedStoredAnnots <- function() {
  if (length(reactiveVars$SelectedStoredAnnotationIds) > 0)
    return(StoredAnnots()[which(
      reactiveVars$StoredAnnotations$id %in%
        reactiveVars$SelectedStoredAnnotationIds
    ),])
  else if (nrow(StoredAnnots()) == 0 || is.na(StoredAnnots()[1,]))
    return(NULL)
  else return(StoredAnnots()[1,])
}

SelectedStoredExpress <- function() {
  if (length(reactiveVars$SelectedStoredExpressionIds) > 0)
    return(StoredExpress()[which(
      reactiveVars$StoredExpressions$id %in%
        reactiveVars$SelectedStoredExpressionIds
    ),])
  else if (nrow(StoredAnnots()) == 0 || is.na(StoredAnnots()[1,]))
    return(NULL)
  else return(StoredAnnots()[1,])
}

fetchDatasetEx <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}

# sub-routines ####
