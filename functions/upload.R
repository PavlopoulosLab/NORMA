# main functions ####
addNetwork <- function(){
  dataset <- loadNetworkFromFile()
  
  if (!is.null(dataset)) {
    if (nrow(dataset) < 10000) {
      nid <- UUIDgenerate(T)      #time-base UUID is generated
      nn <- input$networkName
      cnt <- 1                    #count
      while (nn %in% reactiveVars$StoredNetworks$name) {
        #reactiveVars: represents a single reactive variable.
        cnt <- cnt + 1
        nn <-
          paste(input$networkName, cnt)      #paste: converts its arguments (via as.character) to character strings
      }
      df <- data.frame(id = nid, name = nn, stringsAsFactors = F)
      if (nrow(dataset) > 10000) {
        dataset <- dataset[1:10000,]
      }
      attr(dataset, which = 'weighted') <- input$weighted1
      
      reactiveVars$StoredNetworks <-
        rbind(reactiveVars$StoredNetworks, df)
      reactiveVars$StoredNetworks_just_network <-
        reactiveVars$StoredNetworks
      reactiveVars$StoredNetworks_annotations_tab <-
        reactiveVars$StoredNetworks
      reactiveVars$StoredNetworks_topology_tab <-
        reactiveVars$StoredNetworks
      
      saveRDS(dataset, paste0(nid, ".rda"))
      if (length(reactiveVars$SelectedStoredNetworksIds) == 0) {
        reactiveVars$SelectedStoredNetworksIds <- c(nid)
      }
    }
    
    if (nrow(dataset) >= 10000) {
      dataset <- dataset[1:10000, ]
      shinyalert("Warning!", "Keeping the first 10,000 connections.", type = "error")
    }
    
  } else shinyalert("Error!", "Your input generated a NULL network.\n
                    Please, follow the guidelines at the Help pages to upload a network in the required format.", type = "error")
}

remNetwork <- function(){
  if (!is.null(input$availableNetworks)) {
    reactiveVars$StoredNetworks <-
      reactiveVars$StoredNetworks[-which(reactiveVars$StoredNetworks$id %in%
                                           input$availableNetworks),]
    nr <- nrow(reactiveVars$StoredNetworks)
    if (nr > 0) {
      reactiveVars$SelectedStoredNetworksIds <-
        reactiveVars$StoredNetworks[nr,]$id
    }
  }
  
  if (!is.null(input$availableNetworks)) {
    reactiveVars$StoredNetworks_just_network <-
      reactiveVars$StoredNetworks_just_network[-which(reactiveVars$StoredNetworks_just_network$id %in%
                                                        input$availableNetworks),]
    nr <- nrow(reactiveVars$StoredNetworks_just_network)
    if (nr > 0) {
      reactiveVars$SelectedStoredNetworksIds_just_network <-
        reactiveVars$StoredNetworks_just_network[nr,]$id
    }
  }
  
  if (!is.null(input$availableNetworks)) {
    reactiveVars$StoredNetworks_annotations_tab <-
      reactiveVars$StoredNetworks_annotations_tab[-which(
        reactiveVars$StoredNetworks_annotations_tab$id %in%
          input$availableNetworks
      ),]
    nr <- nrow(reactiveVars$StoredNetworks_annotations_tab)
    if (nr > 0) {
      reactiveVars$SelectedStoredNetworksIds_annotations_tab <-
        reactiveVars$StoredNetworks_annotations_tab[nr,]$id
    }
  }
  
  if (!is.null(input$availableNetworks)) {
    reactiveVars$StoredNetworks_topology_tab <-
      reactiveVars$StoredNetworks_topology_tab[-which(reactiveVars$StoredNetworks_topology_tab$id %in%
                                                        input$availableNetworks),]
    nr <- nrow(reactiveVars$StoredNetworks_topology_tab)
    if (nr > 0) {
      reactiveVars$SelectedStoredNetworksIds_topology_tab <-
        reactiveVars$StoredNetworks_topology_tab[nr,]$id
    }
  }
}

addAnnotations <- function(){
  annotation <- loadAnnotations()
  if (!is.null(annotation)) {
    if (nrow(annotation) < 300) {
      nid <- UUIDgenerate(T)      #time-base UUID is generated
      nn <- input$annotationName
      cnt <- 1                    #count
      while (nn %in% reactiveVars$StoredAnnotations$name) {
        #reactiveVars: represents a single reactive variable.
        cnt <- cnt + 1
        nn <-
          paste(input$annotationName, cnt)      #paste: converts its arguments (via as.character) to character strings
      }
      dtf <- data.frame(id = nid,
                        name = nn,
                        stringsAsFactors = F)
      if (nrow(annotation) > 10000) {
        annotation <- annotation[1:10000,]
      }
      reactiveVars$StoredAnnotations <-
        rbind(reactiveVars$StoredAnnotations, dtf)
      reactiveVars$StoredNetworks2_annotations_tab <-
        reactiveVars$StoredAnnotations
      
      saveRDS(annotation, paste0(nid, ".rda"))
      if (length(reactiveVars$SelectedStoredAnnotationIds) == 0) {
        reactiveVars$SelectedStoredAnnotationIds <- c(nid)
      }
    }
    
    if (nrow(annotation) >= 300) {
      annotation <- annotation[1:300, ]
      shinyalert("Warning!", "Keeping the first 300 lines.", type = "error")
    }
    
  } else shinyalert("Error!", "Your input generated a NULL annotation table.\n
                    Please, follow the guidelines at the Help pages to upload an annotation file in the required format.", type = "error")
}

remAnnotations <- function(){
  if (!is.null(input$availableAnnotations)) {
    reactiveVars$StoredAnnotations <-
      reactiveVars$StoredAnnotations[-which(reactiveVars$StoredAnnotations$id %in%
                                              input$availableAnnotations),]
    nr <- nrow(reactiveVars$StoredAnnotations)
    if (nr > 0)
      reactiveVars$SelectedStoredAnnotationIds <-
      reactiveVars$StoredAnnotations[nr,]$id
  }
  
  
  if (!is.null(input$availableAnnotations)) {
    reactiveVars$StoredNetworks2_annotations_tab <-
      reactiveVars$StoredNetworks2_annotations_tab[-which(
        reactiveVars$StoredNetworks2_annotations_tab$id %in%
          input$availableAnnotations
      ),]
    nr <- nrow(reactiveVars$StoredNetworks2_annotations_tab)
    if (nr > 0)
      reactiveVars$SelectedStoredNetworksIds2_annotations_tab <-
      reactiveVars$StoredNetworks2_annotations_tab[nr,]$id
  }
}

addExpression <- function(){
  expression <- loadExpressions()
  if (!is.null(expression)) {
    nid <- UUIDgenerate(T)      #time-base UUID is generated
    nn <- input$expressionName
    cnt <- 1                    #count
    while (nn %in% reactiveVars$StoredExpressions$name) {
      #reactiveVars: represents a single reactive variable.
      cnt <- cnt + 1
      nn <-
        paste(input$expressionName, cnt)      #paste: converts its arguments (via as.character) to character strings
    }
    dtf <- data.frame(id = nid,
                      name = nn,
                      stringsAsFactors = F)
    if (nrow(expression) > 10000) {
      expressionName <- expressionName[1:10000,]
      shinyalert("Warning!", "Keeping the 10000 first rows.", type = "error")
    }
    reactiveVars$StoredExpressions <-
      rbind(reactiveVars$StoredExpressions, dtf)
    saveRDS(expression, paste0(nid, ".rda"))
    if (length(reactiveVars$SelectedStoredExpressionIds) == 0) {
      reactiveVars$SelectedStoredExpressionIds <- c(nid)
    }
  } else shinyalert("Error!", "Your input generated a NULL expressions table.\n
                    Please, follow the guidelines at the Help pages to upload an annotation file in the required format.", type = "error")
}

remExpression <- function(){
  if (!is.null(input$availableExpressions)) {
    reactiveVars$StoredExpressions <-
      reactiveVars$StoredExpressions[-which(reactiveVars$StoredExpressions$id %in%
                                              input$availableExpressions),]
    nr <- nrow(reactiveVars$StoredExpressions)
    if (nr > 0)
      reactiveVars$SelectedStoredExpressionIds <-
      reactiveVars$StoredExpressions[nr,]$id
  }
}

getStoredNetsChoices <- function() {
  snets <- StoredNets()
  if (nrow(snets) == 0)
    return(NULL)
  choices <- snets$id
  names(choices) <- snets$name
  return(choices)
}

SelectedStoredNets <- function() {
  if (length(reactiveVars$SelectedStoredNetworksIds) > 0) {
    return(StoredNets()[which(reactiveVars$StoredNetworks$id %in%
                                reactiveVars$SelectedStoredNetworksIds),])
  }
  else if (nrow(StoredNets()) == 0 ||
           is.na(StoredNets()[1,]))
    return(NULL)
  else return(StoredNets()[1,])
}

fetchDataset <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
}

getStoredAnnotChoices <- function() {
  sannots <- StoredAnnots()
  if (nrow(sannots) == 0)
    return(NULL)
  choices <- sannots$id
  names(choices) <- sannots$name
  return(choices)
}

# sub-routines ####
loadNetworkFromFile <- function() {
  dataset1 <- NULL
  set.seed(123)
  
  switch(
    input$uiLoadGraphOptionsInput,
    oF = {
      if (!is.null(input$file1)) {
        tryCatch({
          dataset1 <- read_data(input$file1$datapath)
        }, error = function(e) {
          print(paste("Upload tab error: ", e))
          shinyalert("Error!", "Network format problem. Please ensure that columns are tab separated.", type = "error")
        })
      }
    },
    oR_String_interactions = {
      dataset1 <- string_net_bcar3  # "Examples/BCAR3/BCAR3.txt"
      dataset1 <- cbind(dataset1[,1:2], "Weight" = rep(1, nrow(dataset1)))
    },
    oR_Drosophila = {
      n <- as.integer(input$oR_selected_size)
      dataset1 <- dros_net # "Examples/TAU/TAU_network_DEGs_NORMA.txt"
      dataset1 <- cbind(dataset1[,1:2], "Weight" = rep(1, nrow(dataset1)))
    }
  )
  
  if (input$uiLoadGraphOptionsInput != "oF" && !is.null(dataset1)) {
    set.seed(123)
    if(input$weighted1==F) {
      dataset1 <- cbind(dataset1[,1:2], "Weight" = rep(1, nrow(dataset1)))
    } else {
      if (ncol(dataset1) == 2) {
        dataset1$V3 <- 1
      } else if (ncol(dataset1) > 3) {
        dataset1 <- dataset1[, 1:3]
      } else if (ncol(dataset1) != 3) 
        return(NULL)
    }
    colnames(dataset1) <- c("Source", "Target", "Weight")
  }
  
  if (!is.null(dataset1)){
    row_to_keep <- c()
    for(i in 1:nrow(dataset1)){
      if(dataset1[i,1]==dataset1[i,2]){
        row_to_keep <- c(row_to_keep, FALSE) 
      }
      if(dataset1[i,1]!=dataset1[i,2]){
        row_to_keep <- c(row_to_keep, TRUE) 
      }
    }
    dataset1 = dataset1[row_to_keep,]
  }
  
  return(dataset1)
}

# Load annotations
loadAnnotations <- function() {
  annotation1 <- NULL
  
  switch(
    input$uiLoadGraphOptionsInput_annotations,
    oF = {
      if (!is.null(input$file2)) {
        tryCatch({
          annotation1 <- read_annotations(input$file2$datapath)
        }, error = function(e) {
          print(paste("Upload tab error: ", e))
          shinyalert("Error!", "Annotations format problem. Please ensure that columns are tab separated.", type = "error")
        })
      }
    },
    oR_String_Annotation_BP = {
      annotation1 <- string_bp # "Examples/BCAR3/BCAR3_GO_BP.txt"
    }, 
    oR_String_Annotation_MF = {
      annotation1 <- string_mf # "Examples/BCAR3/BCAR3_GO_MF.txt"
    }, 
    oR_String_Annotation_KEGG = {
      annotation1 <- string_kegg # "Examples/BCAR3/BCAR3_KEGG.txt"
    },
    oR_Drosophila_KEGG = {
      # n <- as.integer(input$oR_selected_size)
      annotation1 <- dros_annot # "Examples/TAU/TAU_KEGG_Annotation_NORMA.txt"
    },
    oR_Drosophila_Luvain = {
      # n <- as.integer(input$oR_selected_size)
      annotation1 <- dros_louvain # "Examples/TAU/TAU_Louvain.txt"
    }
  )
  
  if (!is.null(annotation1)) {
    colnames(annotation1) <- c("Annotations", "Nodes")
  }
  
  return(annotation1)
}

# Load expression files
loadExpressions <- function() {
  expression1 <- NULL
  switch(input$uiLoadExpressionsInput,
         oF = {
           if (!is.null(input$file3)) {
             tryCatch({
               expression1 <- read_expressions(input$file3$datapath)
             }, error = function(e) {
               print(paste("Upload tab error: ", e))
               shinyalert("Error!", "Expressions format problem. Please ensure that columns are tab separated.", type = "error")
             })
           }
         },
         oR_Expression_file_Drosophila = {
           expression1 <- dros_express # "Examples/TAU/TAU_expressions.txt"
         }
  )
  if (!is.null(expression1)) {
    colnames(expression1) <- c("ID", "Color")
  }
  return(expression1)
}

read_data <- function(datapath, type = c("txt"), header = T, sep = "\t", quote = "\"", weighted = F){
  dataset1 <- read.table(datapath, header = header, sep = sep, quote = quote)
  
  if (ncol(dataset1) == 2) dataset1$V3 <- 1
  else if (ncol(dataset1) > 3) dataset1 <- dataset1[, 1:3]
  else if (ncol(dataset1) != 3) return(NULL)
  
  colnames(dataset1) <- c("Source", "Target", "Weight")
  
  return(dataset1)
}

read_annotations <- function(datapath, type = c("txt"), header = F, sep = "\t",
                             quote = "\"", weighted = F, na.strings = c("", "NA")){
  annotation1 <- read.table(datapath, header = header, sep = sep, quote = quote)
  
  return(annotation1)
}
