library(shiny)

colors <- randomColor(50)

read_data <-
  function(datapath,
           type = c("txt"),
           header = T,
           sep = "\t",
           quote = "\"",
           weighted = F,
           directed = F)
    ({
      dataset1 <-
        read.table(datapath,
                   header = header,
                   sep = sep,
                   quote = quote)
      # if(weighted)
      if (ncol(dataset1) == 2) {
        dataset1$V3 <- 1
      } else if (ncol(dataset1) > 3) {
        dataset1 <- dataset1[, 1:3]
      } else if (ncol(dataset1) != 3)
        return(NULL)
      
      colnames(dataset1) <- c("Source", "Target", "Weight")
      #else colnames(dataset1) <- c('Source', 'Target')
      return(dataset1)
    })


read_annotations <-
  function(datapath,
           type = c("txt"),
           header = F,
           sep = "\t",
           quote = "\"",
           weighted = F,
           na.strings = c("", "NA"))
    ({
      annotation1 <-
        read.table(datapath,
                   header = header,
                   sep = sep,
                   quote = quote)
      
    })

read_expressions <-
  function(datapath,
           type = c("txt"),
           header = F,
           sep = "\t",
           quote = "\"",
           weighted = F,
           na.strings = c("", "NA"))
    ({
      expression1 <-
        read.table(datapath,
                   header = header,
                   sep = sep,
                   quote = quote)
      
    })

convert_to_igraph <- function(dataset1)
  ({
    # directed_tf <- FALSE
    # if (attr(dataset1, "directed")) {
    #     directed_tf <- attr(dataset1, "directed")
    # }
    igraph <-
      graph.data.frame(dataset1, vertices = NULL, directed = F)
    
    return(simplify(igraph))
  })

EmptyDataset <- function(columns) {
  dataset <- data.frame(V1 = integer())
  lapply(columns[-1], function(x)
    dataset[, x] <<- integer())
  colnames(dataset) <- columns
  return(dataset)
}

ui_options <- c(ui_table_font_sz = "80%") #html size
options(shiny.error = browser) #debugging



shinyServer(function(input, output, session) {
  reactiveVars <- reactiveValues()
  reactiveVars$StoredNetworks <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds <- c()
  #
  reactiveVars$StoredAnnotations <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredAnnotationIds <- c()
  reactiveVars$StoredExpressions <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredExpressionIds <- c()
  #
  reactiveVars$StoredNetworks_just_network <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds_just_network <- c()
  
  session$onSessionEnded(function() {
    snets <- isolate(StoredNets())
    if (nrow(snets) > 0) {
      unlink(c("*.rda", "*.zip"))
    }
  })
  
  session$onSessionEnded(function() {
    sannots <- isolate(StoredAnnots())
    if (nrow(sannots) > 0) {
      unlink(c("*.rda", "*.zip"))
    }
  })
  
  session$onSessionEnded(function() {
    sexpress <- isolate(StoredExpress())
    if (nrow(sexpress) > 0) {
      unlink(c("*.rda", "*.zip"))
    }
  })
  
  ############ Read the files ################
  loadNetworkFromFile <- function() {
    dataset1 <- NULL
    switch(
      input$uiLoadGraphOptionsInput,
      oF = {
        if (!is.null(input$file1)) {
          dataset1 <- read_data(input$file1$datapath)
        }
      },
      oR_String_interactions = {
        dataset1 <-
          read.delim("Examples/BCAR3/BCAR3.txt", header = T)
      },
      oR_Drosophila = {
        n <- as.integer(input$oR_selected_size)
        dataset1 <- read.delim("Examples/TAU/TAU_network_DEGs_NORMA.txt")
      }
    )
    if (input$uiLoadGraphOptionsInput != "oF" &&
        !is.null(dataset1)) {
      # if(input$weighted1) {
      if (!is.null(dataset1))
        dataset1$X3 <-
          sample(1:10, nrow(dataset1), replace = T)
      colnames(dataset1) <- c("Source", "Target", "Weight")
      # } else { colnames(dataset1) <- c('Source', 'Target') }
    }
    return(dataset1)
  }
  
  output$uiLoadGraphOptionsOutput <- renderUI({
    if (is.null(input$uiLoadGraphOptionsInput))
      return()
    
    # Depending on input$input_type, we'll generate a different UI
    # component and send it to the client.
    if (input$uiLoadGraphOptionsInput == "oF") {
      wellPanel(fileInput(
        "file1",
        "Choose file to upload",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ))
    }
    #, checkboxInput(inputId = "directed1", "Directed", value = FALSE)
  })
  
  
  #Load annotations
  loadAnnotations <- function() {
    annotation1 <- NULL
    switch(
      input$uiLoadGraphOptionsInput_annotations,
      oF = {
        if (!is.null(input$file2)) {
          annotation1 <- read_annotations(input$file2$datapath)
        }
      },
      oR_String_Annotation_BP = {
        annotation1 <-read.delim("Examples/BCAR3/BCAR3_GO_BP.txt", header = F)
      }, 
      oR_String_Annotation_MF = {
        annotation1 <-read.delim("Examples/BCAR3/BCAR3_GO_MF.txt", header = F)
      }, 
      oR_String_Annotation_KEGG = {
        annotation1 <-read.delim("Examples/BCAR3/BCAR3_KEGG.txt", header = F)
      },
      oR_Drosophila_KEGG = {
        n <- as.integer(input$oR_selected_size)
        annotation1 <-
          read.delim("Examples/TAU/TAU_KEGG_Annotation_NORMA.txt", header = F)
      },
      oR_Drosophila_Luvain = {
        n <- as.integer(input$oR_selected_size)
        annotation1 <-
          read.delim("Examples/TAU/TAU_Louvain.txt", header = F)
      }
    )
    if (!is.null(annotation1)) {
      colnames(annotation1) <- c("Annotations", "Nodes")
    }
    
    return(annotation1)
  }
  
  output$uiLoadGraphOptionsOutput_annotations <- renderUI({
    if (is.null(input$uiLoadGraphOptionsInput_annotations))
      return()
    
    # Depending on input$input_type, we'll generate a different UI
    # component and send it to the client.
    if (input$uiLoadGraphOptionsInput_annotations == "oF") {
      wellPanel(fileInput(
        "file2",
        "Choose file to upload",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ))
    }
    
  })
  
  ####################################
  
  #Change of Network Name based on input choices
  dochangeNetworkName <- observe({
    updateTextInput(session,
                    inputId = "networkName",
                    value = (if (input$uiLoadGraphOptionsInput == "OF") {
                      paste("Network name")
                    }))
  })
  
  dochangeNetworkName2 <- observe({
    updateTextInput(session,
                    inputId = "networkName",
                    value = (if (input$uiLoadGraphOptionsInput == "oR_String_interactions") {
                      paste("BCAR3 STRING Network")
                    }))
  })
  
  dochangeNetworkName3 <- observe({
    updateTextInput(session,
                    inputId = "networkName",
                    value = (if (input$uiLoadGraphOptionsInput == "oR_Drosophila") {
                      paste("Drosophila TAU Network")
                    }))
  })
  
  #####################################################
  #Change of Annotation Name based on input choices
  dochangeAnnotationName <- observe({
    updateTextInput(session,
                    inputId = "annotationName",
                    value = (if (input$uiLoadGraphOptionsInput_annotations == "OF") {
                      paste("Annotation name")
                    }))
  })
  
  dochangeAnnotationName2a <- observe({
    updateTextInput(session,
                    inputId = "annotationName",
                    value = (if (input$uiLoadGraphOptionsInput_annotations == "oR_String_Annotation_BP") {
                      paste("BCAR3 GO Biological Process")
                    }))
  }) 
  
  dochangeAnnotationName2b <- observe({
    updateTextInput(session,
                    inputId = "annotationName",
                    value = (if (input$uiLoadGraphOptionsInput_annotations == "oR_String_Annotation_MF") {
                      paste("BCAR3 GO Molecular Function")
                    }))
  }) 
  
  dochangeAnnotationName2c <- observe({
    updateTextInput(session,
                    inputId = "annotationName",
                    value = (if (input$uiLoadGraphOptionsInput_annotations == "oR_String_Annotation_KEGG") {
                      paste("BCAR3 GO KEGG pathways")
                    }))
  }) 
  
  dochangeAnnotationName3a <- observe({
    updateTextInput(session,
                    inputId = "annotationName",
                    value = (if (input$uiLoadGraphOptionsInput_annotations == "oR_Drosophila_KEGG") {
                      paste("Drosophila TAU KEGG pathways")
                    }))
  })
  
  dochangeAnnotationName3b <- observe({
    updateTextInput(session,
                    inputId = "annotationName",
                    value = (if (input$uiLoadGraphOptionsInput_annotations == "oR_Drosophila_Luvain") {
                      paste("Drosophila TAU Louvain")
                    }))
  })
  
  dochangeExpressionName <- observe({
    updateTextInput(session,
                    inputId = "expressionName",
                    value = (if (input$uiLoadExpressions == "oR_Expression_file_Drosophila") {
                      paste("Drosophila TAU node - coloring file")
                    }))
  })
  
  
  ######################################################
  StoredNets <- reactive({
    return(reactiveVars$StoredNetworks)
  })
  
  SelectedStoredNets <- function() {
    if (length(reactiveVars$SelectedStoredNetworksIds) > 0) {
      return(StoredNets()[which(reactiveVars$StoredNetworks$id %in%
                                  reactiveVars$SelectedStoredNetworksIds),])
    }
    else if (nrow(StoredNets()) == 0 ||
             is.na(StoredNets()[1,]))
      return(NULL)
    else {
      # updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy", "Selected network(s)", choices = getStoredNetsChoices(), selected = getStoredNetsChoices()[1])
      return(StoredNets()[1,])
    }
  }
  
  StoredNetsEmpty <- function() {
    return(nrow(reactiveVars$StoredNetworks) == 0)
  }
  
  getDatasetName <- function(id) {
    sn <- StoredNets()
    idi <- which(sn$id == id)
    if (is.null(sn) || nrow(sn) == 0 || length(idi) == 0)
      return(NULL)
    return(sn[idi,]$name)
  }
  
  fetchDataset <- function(nid) {
    retVal <- NULL
    if (length(nid) > 0) {
      retVal <- readRDS(paste0(nid, ".rda"))
      attr(retVal, "id") <- nid
    }
    return(retVal)
  }
  
  fetchFirstSelectedStoredDataset <- reactive({
    ssn <- SelectedStoredNets()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      return(fetchDataset(ssn[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  fetchFirstSelectedStoredIgraph <- function() {
    dataset <- fetchFirstSelectedStoredDataset()
    if (is.null(dataset))
      return(NULL)
    else
      return(convert_to_igraph(dataset))
  }
  
  fetchAllSelectedStoredDataset <- function() {
    ssn <- SelectedStoredNets()
    ids <- c()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        ret[[i]] <- fetchDataset(ssn[i,]$id)
        ids <- c(ids, ssn[i,]$id)
      }
      names(ret) <- ids
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  fetchMaxNSelectedStoredDataset <- function(N) {
    ssn <- SelectedStoredNets()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        if (i > N)
          break
        ret[[i]] <- fetchDataset(ssn[i,]$id)
      }
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  fetchMaxTwoSelectedStoredIgraphs <- function() {
    datasets <- fetchMaxNSelectedStoredDataset(2)
    if (is.null(datasets) || length(datasets) == 0)
      return(NULL)
    else {
      ret <- list()
      for (i in length(datasets))
        ret[[i]] <- convert_to_igraph(datasets[[i]])
      return(ret)
    }
  }
  
  fetchFirstSelectedStoredAnnotations <- function() {
    groups_ann <- fetchFirstSelectedStoredGroups()
    if (is.null(groups_ann))
      return(NULL)
    else
      return(groups_ann)
  }
  
  fetchFirstSelectedStoredGroups <- reactive({
    sannots <- SelectedStoredAnnots()
    if (!is.null(sannots) && nrow(sannots) > 0) {
      return(fetchDataset(sannots[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  ###### Upload #######
  doAddNetwork <- observeEvent(input$btnAddNetwork, {
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
        df <-
          data.frame(id = nid,
                     name = nn,
                     stringsAsFactors = F)
        if (nrow(dataset) > 10000) {
          dataset <- dataset[1:10000,]
        }
        # attr(dataset, which = "directed") <- input$directed1
        # attr(dataset, which = 'weighted') <- input$weighted1
        
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
      
      if (nrow(dataset) >= 100000) {
        createAlert(
          session,
          "tabUpload_up_to_10000_rows",
          "fileUploadAlert_up_to_10000_rows",
          title = "Warning !",
          style = "danger",
          content = paste0(
            "Please make sure that your network has less than 10,000 connections."
          ),
          append = FALSE
        )
        dataset <- NULL
      }
      
    } else
      createAlert(
        session,
        "tabUploadSideAlert",
        "fileUploadAlert",
        title = "ERROR !",
        style = "danger",
        content = paste0(
          "An error occurred while trying to read your file. Please make sure that it is formatted according to the requirements."
        ),
        append = FALSE
      )
    
  })
  
  doRemNetwork <- observeEvent(input$btnRemoveNetworks, {
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
  })
  
  getStoredNetsChoices <- function() {
    snets <- StoredNets()
    if (nrow(snets) == 0)
      return(NULL)
    choices <- snets$id
    names(choices) <- snets$name
    return(choices)
  }
  
  output$uiStoredGraphsOutputRadio <- renderUI({
    input$btnAddNetwork
    input$btnRemoveNetworks
    choices <- getStoredNetsChoices()
    if (is.null(choices))
      return()
    return(list(
      br(),
      wellPanel(
        h4("Available networks"),
        checkboxGroupInput(
          "availableNetworks",
          label = "",
          choices = choices
        )
      ),
      div(div(
        actionButton("btnRemoveNetworks", "REMOVE", icon = icon("minus")),
        class = "col-md-5 centerBlock"
      ), class = "row text-center")
    ))
  })
  
  uploadTabSetSelectedNetwork <-
    observeEvent(input$storedGraphsOutputSelectUpload, {
      reactiveVars$SelectedStoredNetworksIds <-
        c(input$storedGraphsOutputSelectUpload)
    }, ignoreNULL = FALSE)
  
  output$uiStoredGraphsOutputSelectUpload <- renderUI({
    input$btnAddNetwork
    input$btnRemoveNetworks
    choices <- getStoredNetsChoices()
    if (is.null(choices))
      return()
    return(selectInput(
      "storedGraphsOutputSelectUpload",
      "Selected network",
      choices
    ))
  })
  
  ### Duplicated upload for networks in network-tab (just_network) ###
  output$uiLoadGraphOptionsOutput_just_network <-  renderUI({
    input$btnAddNetwork
    input$btnRemoveNetworks
    choices <- getStoredNetsChoices_just_network()
    if (is.null(choices))
      return()
    return(
      selectInput(
        "storedGraphsOutputSelectUpload_just_network",
        "Selected network",
        choices
      )
    )
  })
  
  uploadTabSetSelectedNetwork_just_network <-
    observeEvent(input$storedGraphsOutputSelectUpload_just_network,
                 {
                   reactiveVars$SelectedStoredNetworksIds_just_network <-
                     c(input$storedGraphsOutputSelectUpload_just_network)
                 },
                 ignoreNULL = FALSE)
  
  fetchFirstSelectedStoredDataset_just_network <- reactive({
    ssn <- SelectedStoredNets_just_network()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      return(fetchDataset_just_network(ssn[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  fetchFirstSelectedStoredIgraph_just_network <- function() {
    dataset <- fetchFirstSelectedStoredDataset_just_network()
    if (is.null(dataset))
      return(NULL)
    else
      return(convert_to_igraph(dataset))
  }
  
  fetchDataset_just_network <- function(nid) {
    retVal <- NULL
    if (length(nid) > 0) {
      retVal <- readRDS(paste0(nid, ".rda"))
      attr(retVal, "id") <- nid
    }
    return(retVal)
  }
  
  fetchAllSelectedStoredDataset_just_network <- function() {
    ssn <- SelectedStoredNets_just_network()
    ids <- c()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        ret[[i]] <- fetchDataset_just_network(ssn[i,]$id)
        ids <- c(ids, ssn[i,]$id)
      }
      names(ret) <- ids
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  fetchMaxNSelectedStoredDataset_just_network <- function(N) {
    ssn <- SelectedStoredNets_just_network()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        if (i > N)
          break
        ret[[i]] <- fetchDataset_just_network(ssn[i,]$id)
      }
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  getStoredNetsChoices_just_network <- function() {
    snets <- StoredNets_just_network()
    if (nrow(snets) == 0)
      return(NULL)
    choices <- snets$id
    names(choices) <- snets$name
    return(choices)
  }
  
  StoredNets_just_network <- reactive({
    return(reactiveVars$StoredNetworks_just_network)
  })
  StoredNetsEmpty_just_network <- function() {
    return(nrow(reactiveVars$StoredNetworks_just_network) == 0)
  }
  
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
  
  #########################################
  
  ### Duplicated upload for selctInput boxes of networks in annotation-tab (_annotations_tab)###
  reactiveVars$StoredNetworks_annotations_tab <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds_annotations_tab <- c()
  
  fetchFirstSelectedStoredDataset_annotations_tab <- reactive({
    ssn <- SelectedStoredNets_annotations_tab()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      return(fetchDataset_annotations_tab(ssn[1,]$id))
    } else {
      return(NULL)
    }
  })
  
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
  
  fetchAllSelectedStoredDataset_annotations_tab <- function() {
    ssn <- SelectedStoredNets_annotations_tab()
    ids <- c()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        ret[[i]] <- fetchDataset_annotations_tab(ssn[i,]$id)
        ids <- c(ids, ssn[i,]$id)
      }
      names(ret) <- ids
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  fetchMaxNSelectedStoredDataset_annotations_tab <- function(N) {
    ssn <- SelectedStoredNets_annotations_tab()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        if (i > N)
          break
        ret[[i]] <- fetchDataset_annotations_tab(ssn[i,]$id)
      }
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  output$uiLoadGraphOptionsOutput_annotations_tab <-  renderUI({
    input$btnAddNetwork
    input$btnRemoveNetworks
    choices <- getStoredNetsChoices_annotations_tab()
    if (is.null(choices))
      return()
    return(
      selectInput(
        "storedGraphsOutputSelectUpload_annotations_tab",
        "Selected network",
        choices
      )
    )
    
  })
  
  uploadTabSetSelectedNetwork_annotations_tab <-
    observeEvent(input$storedGraphsOutputSelectUpload_annotations_tab,
                 {
                   reactiveVars$SelectedStoredNetworksIds_annotations_tab <-
                     c(input$storedGraphsOutputSelectUpload_annotations_tab)
                 },
                 ignoreNULL = FALSE)
  
  
  getStoredNetsChoices_annotations_tab <- function() {
    snets <- StoredNets_annotations_tab()
    if (nrow(snets) == 0)
      return(NULL)
    choices <- snets$id
    names(choices) <- snets$name
    return(choices)
  }
  
  StoredNets_annotations_tab <- reactive({
    return(reactiveVars$StoredNetworks_annotations_tab)
  })
  StoredNetsEmpty_annotations_tab <- function() {
    return(nrow(reactiveVars$StoredNetworks_annotations_tab) == 0)
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
  
  ### Duplicated upload for annotations in annotation-tab ###
  reactiveVars$StoredNetworks2_annotations_tab <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds2_annotations_tab <- c()
  
  fetchFirstSelectedStoredAnnotations2_annotations_tab <-
    function() {
      groups_ann <- fetchFirstSelectedStoredGroups2_annotations_tab()
      if (is.null(groups_ann))
        return(NULL)
      else
        return(groups_ann)
    }
  
  fetchFirstSelectedStoredGroups2_annotations_tab <- reactive({
    sannots <- SelectedStoredNets2_annotations_tab()
    if (!is.null(sannots) && nrow(sannots) > 0) {
      return(fetchDataset2_annotations_tab(sannots[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  fetchDataset2_annotations_tab <- function(nid) {
    retVal <- NULL
    if (length(nid) > 0) {
      retVal <- readRDS(paste0(nid, ".rda"))
      attr(retVal, "id") <- nid
    }
    return(retVal)
  }
  
  fetchAllSelectedStoredDataset2_annotations_tab <- function() {
    ssn <- SelectedStoredNets2_annotations_tab()
    ids <- c()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        ret[[i]] <- fetchDataset2_annotations_tab(ssn[i,]$id)
        ids <- c(ids, ssn[i,]$id)
      }
      names(ret) <- ids
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  fetchMaxNSelectedStoredDataset2_annotations_tab <- function(N) {
    ssn <- SelectedStoredNets2_annotations_tab()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        if (i > N)
          break
        ret[[i]] <- fetchDataset2_annotations_tab(ssn[i,]$id)
      }
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  output$uiLoadGraphOptionsOutput_annotations_annotations_tab <-  renderUI({
    input$btnAddNetwork2
    input$btnRemoveNetworks2
    choices <- getStoredNetsChoices2_annotations_tab()
    if (is.null(choices))
      return()
    return(
      selectInput(
        "storedGraphsOutputSelectUpload2_annotations_tab",
        "Selected annotation",
        choices
      )
    )
    
  })
  
  uploadTabSetSelectedNetwork2_annotations_tab <-
    observeEvent(input$storedGraphsOutputSelectUpload2_annotations_tab,
                 {
                   reactiveVars$SelectedStoredNetworksIds2_annotations_tab <-
                     c(input$storedGraphsOutputSelectUpload2_annotations_tab)
                 },
                 ignoreNULL = FALSE)
  
  getStoredNetsChoices2_annotations_tab <- function() {
    snets <- StoredNets2_annotations_tab()
    if (nrow(snets) == 0)
      return(NULL)
    choices <- snets$id
    names(choices) <- snets$name
    return(choices)
  }
  
  StoredNets2_annotations_tab <- reactive({
    return(reactiveVars$StoredNetworks2_annotations_tab)
  })
  StoredNetsEmpty2_annotations_tab <- function() {
    return(nrow(reactiveVars$StoredNetworks2_annotations_tab) == 0)
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
  
  #########################################################
  
  ### Add-Annotations button ####
  uploadTabSetSelectedNetwork2 <-
    observeEvent(input$storedGraphsOutputSelectUpload2, {
      reactiveVars$SelectedStoredAnnotationIds <-
        c(input$storedGraphsOutputSelectUpload2)
    }, ignoreNULL = FALSE)
  
  output$uiStoredGraphsOutputRadio_annotations <- renderUI({
    input$btnAddNetwork2
    input$btnRemoveNetworks2
    choices <- getStoredAnnotChoices()
    if (is.null(choices))
      return()
    return(list(
      br(),
      wellPanel(
        h4("Available Annotations"),
        checkboxGroupInput(
          "availableAnnotations",
          label = "",
          choices = choices
        )
      ),
      div(div(
        actionButton("btnRemoveNetworks2", "REMOVE", icon = icon("minus")),
        class = "col-md-5 centerBlock"
      ), class = "row text-center")
    ))
  })
  
  output$uiStoredGraphsOutputSelectUpload2 <- renderUI({
    input$btnAddNetwork2
    input$btnRemoveNetworks2
    choices <- getStoredAnnotChoices()
    if (is.null(choices))
      return()
    return(
      selectInput(
        "storedGraphsOutputSelectUpload2",
        "Selected annotation",
        choices
      )
    )
  })
  
  getStoredAnnotChoices <- function() {
    sannots <- StoredAnnots()
    if (nrow(sannots) == 0)
      return(NULL)
    choices <- sannots$id
    names(choices) <- sannots$name
    return(choices)
  }
  
  StoredAnnots <- reactive({
    return(reactiveVars$StoredAnnotations)
  })
  
  doAddAnnotations <- observeEvent(input$btnAddNetwork2, {
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
        createAlert(
          session,
          "tabUpload_up_to_10000_rows",
          "fileUploadAlert_up_to_10000_rows",
          title = "Warning !",
          style = "danger",
          content = paste0(
            "Please make sure that your annotation file has less than 300 lines."
          ),
          append = FALSE
        )
        annotation <- NULL
      }
      
    } else
      createAlert(
        session,
        "tabUploadSideAlert",
        "fileUploadAlert",
        title = "ERROR !",
        style = "danger",
        content = paste0(
          "An error occurred while trying to read your file. Please make sure that it is formatted according to the requirements (Check tabs between columns."
        ),
        append = FALSE
      )
  })
  
  doRemAnnotations <- observeEvent(input$btnRemoveNetworks2, {
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
  })
  
  ### Output after choosing selected annotations ###
  SelectedStoredAnnots <- function() {
    if (length(reactiveVars$SelectedStoredAnnotationIds) > 0)
      return(StoredAnnots()[which(
        reactiveVars$StoredAnnotations$id %in%
          reactiveVars$SelectedStoredAnnotationIds
      ),])
    else if (nrow(StoredAnnots()) == 0 || is.na(StoredAnnots()[1,]))
      return(NULL)
    else {
      # updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy2", "Selected annotation(s)", choices = getStoredAnnotChoices(), selected = getStoredAnnotChoices()[1])
      return(StoredAnnots()[1,])
    }
  }
  
  fetchFirstSelectedStoredDataset2 <- reactive({
    sannots <- SelectedStoredAnnots()
    if (!is.null(sannots) && nrow(sannots) > 0) {
      return(fetchDataset(sannots[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  ### Table view of Annotations - Upload tab ###
  output$datasettab2 <- DT::renderDataTable({
    annotation <- fetchFirstSelectedStoredDataset2()
    if (is.null(annotation))
      annotation <- EmptyDataset(c("Annotations", "Nodes"))
    datatable(annotation, rownames = FALSE, extensions = 'Responsive') %>% formatStyle(colnames(annotation), fontSize = ui_options["ui_table_font_sz"])
  })
  
  ######## Plots#######################
  ### Interactive Network ###
  output$tabVizIgraphSimple <- renderVisNetwork({
    g <- fetchFirstSelectedStoredIgraph_just_network()
    if (is.null(g))
      return()
    set.seed(123)
    my_network <- as.data.frame(get.edgelist(g))
    my_network <-
      data.frame(from = my_network$V1, to = my_network$V2)
    
    withProgress(min = 0, max = 1, {
      incProgress(message = "Processing data into plot",
                  detail = "This may take a while...",
                  amount = .1)
      visIgraph(as.undirected(g)) %>%
        visNodes(size = 25, shape = "ellipse") %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE) %>%
        visInteraction(
          keyboard = TRUE,
          navigationButtons = TRUE,
          zoomView = TRUE,
          multiselect = TRUE,
          dragView = TRUE
        )
    })
  })
  
  ### Scaling - Sliders###
  scaling_coordinates_convex <- reactive({
    input$scaling_coordinates_convex
  })
  
  scaling_coordinates_pies <- reactive({
    input$scaling_coordinates_pies
  })
  
  scaling_nodes_convex <- reactive({
    input$scaling_nodes_convex
  })
  
  scaling_nodes_pies <- reactive({
    input$scaling_nodes_pies
  })
  
  scaling_labels_convex <- reactive({
    input$scaling_labels_convex
  })
  
  scaling_labels_pies <- reactive({
    input$scaling_labels_pies
  })
  
  ### Pie - Charts ###
  output$chooseGroups2 <- DT::renderDataTable({
    annotation <- fetchFirstSelectedStoredGroups2_annotations_tab()
    if (is.null(annotation))
      annotation <- EmptyDataset(c("Annotations", "Nodes"))
    rowCallback_generated <-
      "function(row, dat, displayNum, index){"
    for (i in 1:length(rownames(annotation)))
    {
      rowCallback_generated <-
        paste(
          rowCallback_generated,
          "if(dat[0]==",
          i,
          "){",
          "$('td:eq(1)', row).addClass('x",
          i,
          "');}" ,
          sep = ""
        )
    }
    rowCallback_generated <-
      paste(rowCallback_generated, "}", sep = "")
    
    css_colors <- group_pal_rows(length(rownames(annotation)))
    
    x <- length(rownames(annotation))
    tmp_css_colors <- c()
    for (i in 1:x)
    {
      tmp_css_colors <- c(tmp_css_colors, css_colors[i])
    }
    datatable(
      annotation,
      extensions = 'Scroller',
      options = list(
        rownames = T,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        rowCallback = JS(rowCallback_generated)
      )
    )
  })
  
  expression_colors_pies <- T
  show_labels_pies <- T
  some_labels_pies <- T
  layouts_with_virtual_nodes_pies<- T
  
  output$tabVizPie_charts <- renderUI({
    s = input$chooseGroups2_rows_selected
    
    g <- fetchFirstSelectedStoredIgraph_annotations_tab()
    annoation_graph <-
      fetchFirstSelectedStoredGroups2_annotations_tab()
    
    if (is.null(g) | is.null(annoation_graph))
      return(NULL)
    
    if (input$layouts_with_virtual_nodes_pies == T) {
      layouts_with_virtual_nodes_pies = T
    }
    else if (input$layouts_with_virtual_nodes_pies == F) {
      layouts_with_virtual_nodes_pies = F
    }
    if (input$show_labels_pies == T) {
      show_labels_pies = T
    }
    else if (input$show_labels_pies == F) {
      show_labels_pies = F
    }
    
    if (input$expressions_pies == T) {
      expression_colors_pies = T
    }
    else if (input$expressions_pies == F) {
      expression_colors_pies = F
    }
    
    if (input$some_labels_pies == T) {
      some_labels_pies = T
    }
    else if (input$some_labels_pies == F) {
      some_labels_pies = F
    }
    
    withProgress(min = 0, max = 1, {
      incProgress(message = "Processing data into plot",
                  detail = "This may take a while...",
                  amount = .1)
      source("interactive_pie_charts.R", local = T)
      lay <- input$layouts2
      pie_charts()
      tags$iframe(
        srcdoc = paste(readLines(
          paste("output_pies_", Sys.getpid(), ".html", sep = "")
        ), collapse = '\n'),
        width = "100%",
        height = "850px"
      )
    })
  })
  
  ### Convex Hull ###
  output$chooseGroups <- DT::renderDataTable({
    annotation <- fetchFirstSelectedStoredGroups2_annotations_tab()
    if (is.null(annotation))
      annotation <- EmptyDataset(c("Annotations", "Nodes"))
    rowCallback_generated <-
      "function(row, dat, displayNum, index){"
    for (i in 1:length(rownames(annotation)))
    {
      rowCallback_generated <-
        paste(
          rowCallback_generated,
          "if(dat[0]==",
          i,
          "){",
          "$('td:eq(1)', row).addClass('x",
          i,
          "');}" ,
          sep = ""
        )
    }
    rowCallback_generated <-
      paste(rowCallback_generated, "}", sep = "")
    
    css_colors <- group_pal_rows(length(rownames(annotation)))
    
    x <- length(rownames(annotation))
    tmp_css_colors <- c()
    for (i in 1:x)
    {
      tmp_css_colors <- c(tmp_css_colors, css_colors[i])
    }
    datatable(
      annotation,
      extensions = 'Scroller',
      options = list(
        rownames = T,
        deferRender = TRUE,
        scrollY = 200,
        scroller = TRUE,
        rowCallback = JS(rowCallback_generated)
      )
    )
  })
  
  expression_colors <- T
  show_labels <- T
  some_labels <- T
  layouts_with_virtual_nodes<- T
  
  output$interactive_convex_hulls <- renderUI({
    s = input$chooseGroups_rows_selected
    
    g <- fetchFirstSelectedStoredIgraph_annotations_tab()
    annoation_graph <-
      fetchFirstSelectedStoredGroups2_annotations_tab()
    if (is.null(g) | is.null(annoation_graph))
      return(NULL)
    
    if (input$layouts_with_virtual_nodes == T) {
      layouts_with_virtual_nodes = T
    }
    else  if (input$layouts_with_virtual_nodes == F) {
      layouts_with_virtual_nodes = F
    }
    if (input$show_labels == T) {
      show_labels = T
    }
    else  if (input$show_labels == F) {
      show_labels = F
    }
    
    if (input$expressions == T) {
      expression_colors = T
    }
    else if (input$expressions == F) {
      expression_colors = F
    }
    if (input$some_labels == T) {
      some_labels = T
    }
    else if (input$some_labels == F) {
      some_labels = F
    }
    
    withProgress(min = 0, max = 1, {
      incProgress(message = "Processing data into plot",
                  detail = "This may take a while...",
                  amount = .1)
      source("interactive_convex_hulls.R", local = T)
      lay <- input$layouts
      convex_hulls()
      
      tags$iframe(
        srcdoc = paste(readLines(
          paste("output_convex_", Sys.getpid(), ".html", sep = "")
        ), collapse = '\n'),
        width = "100%",
        height = "850px"
      )
    })
  })
  
  uiOutputTextError<- 
  
  #######################################################################
  #Expression
  ## Fetch etc ##
  
  getStoredExpressionChoices <- function() {
    sexpress <- StoredExpress()
    if (nrow(sexpress) == 0)
      return(NULL)
    choices <- sexpress$id
    names(choices) <- sexpress$name
    return(choices)
  }
  
  StoredExpressEmpty <- function() {
    return(nrow(reactiveVars$StoredExpressions) == 0)
  }

  StoredExpress <- reactive({
    return(reactiveVars$StoredExpressions)
  })
  
  getDatasetNameEx <- function(id) {
    sn <- StoredExpress()
    idi <- which(sn$id == id)
    if (is.null(sn) || nrow(sn) == 0 || length(idi) == 0)
      return(NULL)
    return(sn[idi,]$name)
  }
  
  SelectedStoredExpress <- function() {
    if (length(reactiveVars$SelectedStoredExpressionIds) > 0)
      return(StoredExpress()[which(
        reactiveVars$StoredExpressions$id %in%
          reactiveVars$SelectedStoredExpressionIds
      ),])
    else if (nrow(StoredAnnots()) == 0 || is.na(StoredAnnots()[1,]))
      return(NULL)
    else {
      # updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy3", "Selected expression(s)", choices = getStoredExpressionChoices(), selected = getStoredExpressionChoices()[1])
      return(StoredAnnots()[1,])
    }
  }
  
  fetchFirstSelectedStoredExpression <- reactive({
    sexpress <- SelectedStoredExpress()
    if (!is.null(sexpress) && nrow(sexpress) > 0) {
      return(fetchDatasetEx(sexpress[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  fetchDatasetEx <- function(nid) {
    retVal <- NULL
    if (length(nid) > 0) {
      retVal <- readRDS(paste0(nid, ".rda"))
      attr(retVal, "id") <- nid
    }
    return(retVal)
  }
  
  fetchFirstSelectedStoredDatasetEx <- reactive({
    sexpress <- SelectedStoredExpress()
    if (!is.null(sexpress) && nrow(sexpress) > 0) {
      return(fetchDatasetEx(sexpress[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  fetchFirstSelectedStoredGroupsEx <- reactive({
    sexpress <- SelectedStoredExpress()
    if (!is.null(sexpress) && nrow(sexpress) > 0) {
      return(fetchDatasetEx(sexpress[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  # Load expression files
  loadExpressions <- function() {
    expression1 <- NULL
    switch(input$uiLoadExpressions,
           oF = {
             if (!is.null(input$file3)) {
               expression1 <- read_expressions(input$file3$datapath)
             }
           },
           # oR_Expression_file_STRING = {
           #   expression1 <-
           #     read.delim("Examples/", header = F)
           # },
           oR_Expression_file_Drosophila = {
             expression1 <-
               read.delim("Examples/TAU/TAU_expressions.txt", header = F)
           }
           )
    if (!is.null(expression1)) {
      colnames(expression1) <- c("ID", "Color")
    }
    return(expression1)
  }
  
  output$uiLoadExpressions <- renderUI({
    if (is.null(input$uiLoadExpressions))
      return()
    
    # Depending on input$input_type, we'll generate a different UI
    # component and send it to the client.
    if (input$uiLoadExpressions == "oF") {
      wellPanel(fileInput(
        "file3",
        "Choose file to upload",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ))
    }
  })
  
  doAddExpression <- observeEvent(input$btnAddExpression, {
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
      }
      reactiveVars$StoredExpressions <-
        rbind(reactiveVars$StoredExpressions, dtf)
      saveRDS(expression, paste0(nid, ".rda"))
      if (length(reactiveVars$SelectedStoredExpressionIds) == 0) {
        reactiveVars$SelectedStoredExpressionIds <- c(nid)
      }
    } else
      createAlert(
        session,
        "tabUploadSideAlert",
        "fileUploadAlert",
        title = "ERROR !",
        style = "danger",
        content = paste0(
          "An error occurred while trying to read your file. Please make sure that it is formatted according to the requirements."
        ),
        append = FALSE
      )
  })
  
  doRemExpression <- observeEvent(input$btnRemoveExpression, {
    if (!is.null(input$availableExpressions)) {
      reactiveVars$StoredExpressions <-
        reactiveVars$StoredExpressions[-which(reactiveVars$StoredExpressions$id %in%
                                                input$availableExpressions),]
      nr <- nrow(reactiveVars$StoredExpressions)
      if (nr > 0)
        reactiveVars$SelectedStoredExpressionIds <-
        reactiveVars$StoredExpressions[nr,]$id
    }
  })
  
  uploadTabSetSelectedExpression <-
    observeEvent(input$uiStoredGraphsOutputSelectUploadExpressions,
                 {
                   reactiveVars$SelectedStoredExpressionIds <-
                     c(input$uiStoredGraphsOutputSelectUploadExpressions)
                 },
                 ignoreNULL = FALSE)
  
  output$uiStoredGraphsOutputRadioEx <- renderUI({
    input$btnAddExpression
    input$btnRemoveExpression
    choices <- getStoredExpressionChoices()
    if (is.null(choices))
      return()
    return(list(
      br(),
      wellPanel(
        h4("Available Expressions"),
        checkboxGroupInput(
          "availableExpressions",
          label = "",
          choices = choices
        )
      ),
      div(div(
        actionButton("btnRemoveExpression", "REMOVE", icon = icon("minus")),
        class = "col-md-5 centerBlock"
      ), class = "row text-center")
    ))
    
  })
  
  output$uiStoredGraphsOutputSelectUploadExpressions <- renderUI({
    input$btnAddExpression
    input$btnRemoveExpression
    choices <- getStoredExpressionChoices()
    if (is.null(choices))
      return()
    return(
      selectInput(
        "uiStoredGraphsOutputSelectUploadExpressions",
        "Selected Expression",
        choices
      )
    )
  })
  
  ######## Basic Info ################
  output$info_vertices1 <- renderText({
    igraph <- fetchFirstSelectedStoredIgraph()
    if (is.null(igraph))
      return()
    igraphName <- SelectedStoredNets()$name
    paste("Number of nodes in", igraphName, ":", vcount(igraph))
  })
  output$info_edges1 <- renderText({
    igraph <- fetchFirstSelectedStoredIgraph()
    if (is.null(igraph))
      return()
    igraphName <- SelectedStoredNets()$name
    paste("Number of intercations in", igraphName, ":", ecount(igraph))
  })
  
  output$vertices1_label <- renderText({
    igraph <- fetchFirstSelectedStoredIgraph()
    if (is.null(igraph))
      return()
    igraphName <- SelectedStoredNets()$name
    paste("Number of vertices in", igraphName, ":", vcount(igraph))
  })
  
  # Print the vertices
  output$vertices1 <- renderPrint({
    igraph <- fetchFirstSelectedStoredIgraph()
    if (is.null(igraph))
      return(invisible(""))
    cat(sort(V(igraph)$name, decreasing = FALSE), sep = "\t")
  })
  
  output$edges1_label <- renderText({
    igraph <- fetchFirstSelectedStoredIgraph()
    if (is.null(igraph))
      return()
    igraphName <- SelectedStoredNets()$name
    paste("Number of edges in", igraphName, ":", ecount(igraph))
  })
  
  output$edges1 <- renderPrint({
    igraph <- fetchFirstSelectedStoredIgraph()
    if (is.null(igraph))
      return(invisible(""))
    # if (as.logical(is.directed(igraph))) {
    #     edgesym <- "<U+2794>"
    # }
    else {
      edgesym <- "<U+2015>"
    }
    
    cat(gsub("\\|", edgesym, attr(E(igraph), "vnames")), sep = "\t")
    # get.edgelist(igraph) get.data.frame(igraph, what=c( 'edges'))
  })
  
  output$diameter1 <- renderText({
    igraph <- fetchFirstSelectedStoredIgraph()
    if (is.null(igraph))
      return()
    diameter <- diameter(igraph)
    paste("Diameter of Graph 1:", diameter)
    # unique_vertices <- length(vertices)
  })
  
  ########## Table format - No loops allowed (simplify) ####################
  
  output$datasettab1 <- DT::renderDataTable({
    dataset <- fetchFirstSelectedStoredDataset()
    if (is.null(dataset))
      dataset <- EmptyDataset(c("Source", "Target", "Weight"))
    else{
      gg <- convert_to_igraph(dataset)
      dataset <- as.data.frame(get.edgelist(gg))
      dataset <- cbind(dataset, rep(1, nrow(dataset)))
      colnames(dataset) <- c("Source", "Target", "Weight")
    }
    return(
      datatable(
        dataset,
        options = list(columnDefs = list(list(
          visible = FALSE, targets = c(2)
        ))),
        rownames = FALSE,
        editable = F
      ) %>% formatStyle(colnames(dataset), fontSize = ui_options["ui_table_font_sz"]) %>% formatRound("Weight")
    )
    
  })
  
  ### Topology tab ###
  reactiveVars$StoredNetworks_topology_tab <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds_topology_tab <- c()
  
  fetchFirstSelectedStoredDataset_topology_tab <- reactive({
    ssn <- SelectedStoredNets_topology_tab()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      return(fetchDataset_topology_tab(ssn[1,]$id))
    } else {
      return(NULL)
    }
  })
  
  fetchFirstSelectedStoredStoredNetworks_topology_tab <-
    function() {
      topol_nets <- fetchFirstSelectedStoredStoredNetworks_topology_tab2()
      if (is.null(topol_nets))
        return(NULL)
      else
        return(topol_nets)
    }
  
  fetchFirstSelectedStoredStoredNetworks_topology_tab2 <-
    reactive({
      stopol <- SelectedStoredNets_topology_tab()
      if (!is.null(stopol) && nrow(stopol) > 0) {
        return(fetchDataset_topology_tab(stopol[1,]$id))
      } else {
        return(NULL)
      }
    })
  
  fetchFirstSelectedStoredIgraph_topology_tab <- function() {
    dataset <- fetchFirstSelectedStoredDataset_topology_tab()
    if (is.null(dataset))
      return(NULL)
    else
      return(convert_to_igraph(dataset))
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
  
  fetchMaxNSelectedStoredDataset_topology_tab <- function(N) {
    ssn <- SelectedStoredNets_topology_tab()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      ret <- list()
      for (i in 1:nrow(ssn)) {
        if (i > N)
          break
        ret[[i]] <- fetchDataset_topology_tab(ssn[i,]$id)
      }
      return(ret)
    } else {
      return(NULL)
    }
  }
  
  getStoredNetsChoices_topology_tab <- function() {
    snets <- StoredNets_topology_tab()
    if (nrow(snets) == 0)
      return(NULL)
    choices <- snets$id
    names(choices) <- snets$name
    return(choices)
  }
  
  StoredNets_topology_tab <- reactive({
    return(reactiveVars$StoredNetworks_topology_tab)
  })
  StoredNetsEmpty_topology_tab <- function() {
    return(nrow(reactiveVars$StoredNetworks_topology_tab) == 0)
  }
  
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
  
  topologyTableViewSetSelectedNetwork <- observeEvent({
    input$storedGraphsOutputSelectTopolopgy
    input$statisticsMethodsMainTabsetPanel
  }, {
    if (input$statisticsMethodsMainTabsetPanel == "tableView" &&
        !(
          length(reactiveVars$SelectedStoredNetworksIds_topology_tab) ==
          1 &&
          reactiveVars$SelectedStoredNetworksIds_topology_tab == input$storedGraphsOutputSelectTopolopgy
        )) {
      reactiveVars$SelectedStoredNetworksIds_topology_tab <-
        c(input$storedGraphsOutputSelectTopolopgy)
    }
  }, ignoreNULL = FALSE)
  
  
  output$uiStoredGraphsOutputSelectTopolopgy <- renderUI({
    input$btnAddNetwork
    input$btnRemoveNetworks
    choices <- getStoredNetsChoices_topology_tab()
    if (is.null(choices))
      return()
    return(selectInput(
      "storedGraphsOutputSelectTopolopgy",
      "Selected network",
      choices
    ))
  })
  
  topologyPlotViewSetSelectedNetwork <- observeEvent({
    input$storedGraphsOutputMultipleSelectTopolopgy
    input$statisticsMethodsMainTabsetPanel
  }, {
    if (input$statisticsMethodsMainTabsetPanel == "plotView" &&
        !(
          length(reactiveVars$SelectedStoredNetworksIds_topology_tab) ==
          length(input$storedGraphsOutputMultipleSelectTopolopgy) &&
          all(
            reactiveVars$SelectedStoredNetworksIds_topology_tab == input$storedGraphsOutputMultipleSelectTopolopgy
          )
        )) {
      reactiveVars$SelectedStoredNetworksIds_topology_tab <-
        c(input$storedGraphsOutputMultipleSelectTopolopgy)
    }
  }, ignoreNULL = FALSE)
  
  
  output$uiStoredGraphsOutputMultipleSelectTopolopgy <- renderUI({
    input$btnAddNetwork
    input$btnRemoveNetworks
    choices <- getStoredNetsChoices_topology_tab()
    if (is.null(choices))
      return()
    return(
      checkboxGroupInput(
        "storedGraphsOutputMultipleSelectTopolopgy",
        "Selected network(s)",
        choices,
        selected = getStoredNetsChoices_topology_tab()[1]
      )
    )
  })
  
  ################ Statistics ########
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
  output$statres <- DT::renderDataTable({
    dset <- fetchFirstSelectedStoredDataset_topology_tab()
    stat_dataset2(dset, getDatasetName(attr(dset, "id")))
  }, options = list(
    paging = FALSE,
    dom = "t",
    rowCallback = JS(
      "function(row, data) {",
      "var num = parseFloat(data[2]).toFixed(2);",
      "if(isNaN(num)){num = data[2];}",
      "$('td:eq(2)', row).html(num);",
      "}"
    )
  ))
  
  doRefreshPalette <- observeEvent(input$btnRefreshPalette, {
    ncolors <- length(colors)
    colors <<- randomColor(ncolors)
  })
  
  output$statisticsMethodsBarPlot <- renderPlot({
    input$btnRefreshPalette
    datasets <- fetchAllSelectedStoredDataset_topology_tab()
    if (length(datasets) == 0)
      return()
    stat_res <- NULL
    for (dataset_i in 1:length(datasets)) {
      dataset <- datasets[[dataset_i]]
      datasetName <-
        getDatasetName(names(datasets)[dataset_i])
      dataset_stat_res <- stat_dataset(dataset, "")
      if (is.null(dataset_stat_res) ||
          nrow(dataset_stat_res) ==
          0)
        next
      if (is.null(stat_res)) {
        stat_res <- dataset_stat_res
        stat_res$network <- datasetName
      } else {
        dataset_stat_res$network <- datasetName
        stat_res <- rbind(stat_res, dataset_stat_res)
      }
    }
    if (is.null(stat_res))
      return()
    colnames(stat_res) <- c("statistic", "value", "network")
    stat_res$value <- as.numeric(as.character(stat_res$value))
    stat_res$network <- factor(stat_res$network)
    ncolors <- length(levels(stat_res$network))
    if (length(colors) < ncolors)
      colors <<- randomColor(ncolors)
    plot.settings <-
      list(
        superpose.polygon = list(col = colors[1:ncolors], border = "transparent"),
        strip.border = list(col = "black")
      )
    barchart(
      value ~ statistic,
      group = network,
      data = stat_res,
      origin = 0,
      horizontal = F,
      auto.key = list(
        space = "right",
        points = F,
        rectangles = T
      ),
      par.settings = plot.settings,
      scales = list(x = list(rot = 45)),
      panel = function(x, y, ...) {
        panel.grid(h = -1, v = 0, col = "gray")
        panel.barchart(x, y, ...)
      }
    )
  })
  
  output$statisticsMethodsPlotRender <- renderUI({
    plotOutput("statisticsMethodsBarPlot", height = paste0(as.integer(
      480 +
        480 * (input$statisticsPlotPercentMagnify / 100)
    ), "px"))
  })
  
  doStatSelectAll <- observeEvent(input$btnStatSelectAll, {
    updateCheckboxGroupInput(session, "statistics2", selected = statistics)
  })
  
  doStatSelectNone <- observeEvent(input$btnStatSelectNone, {
    updateCheckboxGroupInput(session, "statistics2", selected = character(0))
  })
  
  # ######## Modularity Tab #####
  
  show_labels_algorithms_tab <- T
  
  output$modularity_plot <- renderPlot({
    automated_annotations <- input$automated_annotations
    net <- fetchFirstSelectedStoredIgraph_just_network()
    if (is.null(net))
      return()
    clp <-
      automated_annotation_choices(net, automated_annotations)
    
    if (input$show_labels_algorithms_tab == F) {
      show_labels_algorithms_tab = F
      set.seed(123)
      withProgress(min = 0, max = 1, {
        incProgress(message = "Processing data into plot",
                    detail = "This may take a while...",
                    amount = .1)
        plot(
          clp,
          net,
          edge.color = 'grey50',
          mark.col = qual_col_pals,
          vertex.size = 5,
          # vertex.label.color = "black",
          vertex.color = 'grey50',
          vertex.label = NA
        )
      })
    }
    
    else{
      set.seed(123)
      withProgress(min = 0, max = 1, {
        incProgress(message = "Processing data into plot",
                    detail = "This may take a while...",
                    amount = .1)
        plot(
          clp,
          net,
          edge.color = 'grey50',
          mark.col = qual_col_pals,
          vertex.size = 5,
          vertex.label.color = "black",
          vertex.color = 'grey50'
        )
      })
    }
    
  })
  
  output$Modularity_table <- DT::renderDataTable({
    net <- fetchFirstSelectedStoredIgraph_just_network()
    if (is.null(net)) {
      df <- EmptyDataset(c("Annotations", "Nodes"))
    }
    else{
      automated_annotations <- input$automated_annotations
      source("modularity.R", local = T)
      df <- modularity()
      datatable(
        df,
        colnames = c("Annotations", "Nodes"),
        rownames = FALSE,
        extensions = 'Responsive'
      ) %>% formatStyle(colnames(df), fontSize = ui_options["ui_table_font_sz"])
    }
    
  })
  
  ###########################################################
  ## Download-button for automated annotations
  
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
  
  datasetInput <- reactive({
    automated_annotations <- input$automated_annotations
    
    source("modularity.R", local = T)
    net <- fetchFirstSelectedStoredIgraph_just_network()
    if (is.null(net))
      return()
    modularity()
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      automated_annotations <- input$automated_annotations
      paste(file_name(), Sys.Date(), '.txt', sep = '')
    },
    content = function(file) {
      write.table(
        datasetInput(),
        file,
        row.names = FALSE,
        col.names = F,
        quote = F,
        sep = "\t"
      )
    }
  )

  
  ### Venn Diagrams ###
  output$vennDiagram1<- renderUI({
    source("vennDiagrams.R", local=T)
    venn<-vennDiagrams()
    selectInput("node_1", label = "Node 1", choices = c("-",venn$id), selected = "-")
  })
  output$vennDiagram2<- renderUI({
    source("vennDiagrams.R", local=T)
    venn<-vennDiagrams()
    selectInput("node_2", label = "Node 2", choices = c("-",venn$id), selected = "-")
  }) 
  
  area1<- NULL
  area2<- NULL
  v1<- NULL
  v2<- NULL

  output$vennDiagrams <- renderPlot({
    node1_choice<- observe({
      node1 <- input$node_1})
    
    if(!is.null(input$node_1)){
      source("vennDiagrams.R", local = T)
      venn<- vennDiagrams()
      for(i in length(venn$id)){
        venn_i<-which(venn$id==input$node_1)
        v1 <- unlist(stri_split(venn[venn_i,2],fixed=','))
        area1<-length(v1)
      }
    }
    
    node2_choice<- observe({
      node2 <- input$node_2})
    
    if(!is.null(input$node_2)){
      source("vennDiagrams.R", local = T)
      venn<- vennDiagrams()
      for(i in length(venn$id)){
        venn_i<-which(venn$id==input$node_2)
        v2 <- unlist(stri_split(venn[venn_i,2],fixed=','))
        area2<-length(v2)
      }
    }
    
    v12<-length(which(v1 %in% v2))
    
    length_of_gaps<- c(input$node_1, input$node_2)
    if(!is.null(length_of_gaps)){
    empty_length<- length(which(length_of_gaps %in% "-"))
    
    if(empty_length==0){ # 2 nodes
      if( (input$node_1 != input$node_2) )
      {
        draw.pairwise.venn(area1 = area1, 
                           area2 = area2, 
                           cross.area = v12,
                           category = c(input$node_1, input$node_2), 
                           lty = "blank", 
                           fill = c("skyblue", "pink1"))   
    }
    }
    }
    })
  
  node1_choice<- observe({
    node1 <- input$node_1
    node2 <- input$node_2
    
    source("vennDiagrams.R", local = T)
    venn<- vennDiagrams()
    if(is.null(input$node1))
      df<- EmptyDataset(c("V1", "V2"))

    if(!is.null(input$node_1)){
      for(i in length(venn$id)){
        venn_1<-which(venn$id==input$node_1)
        venn_2<-which(venn$id==input$node_2)
        v2 <- unlist(stri_split(venn[venn_2,2],fixed=','))
        v1 <- unlist(stri_split(venn[venn_1,2],fixed=','))
        area1<-length(v1)

        df <- cbind("Node 1"=input$node_1,"Unique Groups"= v1[which((v1%in%v2)==F)])
        df<- as.data.frame(df)
      }
    }
    output$venn_table1 <- DT::renderDataTable({
    datatable(df,rownames = FALSE, extensions = 'Responsive') %>% formatStyle(colnames(df), fontSize = ui_options["ui_table_font_sz"])
  })
  })

  node2_choice<- observe({
    node2 <- input$node_2
    node1 <- input$node_1
    
    source("vennDiagrams.R", local = T)
    venn<- vennDiagrams()
    if(is.null(input$node2))
      df<- EmptyDataset(c("V1", "V2"))

    if(!is.null(input$node_2)){
      for(i in length(venn$id)){
        venn_1<-which(venn$id==input$node_1)
        venn_2<-which(venn$id==input$node_2)
        v2 <- unlist(stri_split(venn[venn_2,2],fixed=','))
        v1 <- unlist(stri_split(venn[venn_1,2],fixed=','))
        area2<-length(v2)
        
        v1 %in% v2
        
        df <- cbind("Node 2"=input$node_2, "Unique Groups"=v2[which((v2%in%v1)==F)])
        df<- as.data.frame(df)
      }
    }
    output$venn_table2 <- DT::renderDataTable({
    datatable(df,rownames = FALSE, extensions = 'Responsive') %>% formatStyle(colnames(df), fontSize = ui_options["ui_table_font_sz"])
  })
  })
  
  output$venn_table_summ <- DT::renderDataTable({
    source("vennDiagrams.R", local = T)
    venn<- vennDiagrams()
    length_of_gaps<- c(input$node_1, input$node_2, input$node_3)
    empty_length<- length(which(length_of_gaps %in% "-"))

    if(is.null(input$node_1)|is.null(input$node_2))
      df<- EmptyDataset(c("Node 1", "Node 2", "Intersection"))
    
          for(i in length(venn$id)){
            venn_1<-which(venn$id==input$node_1)
            venn_2<-which(venn$id==input$node_2)
            v1 <- unlist(stri_split(venn[venn_1,2],fixed=','))
            v2 <- unlist(stri_split(venn[venn_2,2],fixed=','))
            area1<-length(v1)
            area2<-length(v2)
            v12<-length(which(v1 %in% v2))

            df <- cbind("Node 1"=input$node_1, "Node 2"=input$node_2, "Intersection"=paste(v1[which(v1 %in% v2)], sep = ""))
            df<- as.data.frame(df)
          }
          datatable(df,rownames = FALSE, extensions = 'Responsive') %>% formatStyle(colnames(df), fontSize = ui_options["ui_table_font_sz"])
  })
  
  
  #### Help pages - Download files ###
  
  dros_net <- read.delim("Examples/TAU/TAU_network_DEGs_NORMA.txt", header = T)
  dros_annot <- read.delim("Examples/TAU/TAU_KEGG_Annotation_NORMA.txt", header = F)
  dros_louvain <- read.delim("Examples/TAU/TAU_Louvain.txt", header = F)
  dros_express <- read.delim("Examples/TAU/TAU_expressions.txt", header = F)
  output$dros_net <- downloadHandler(
    filename = function() {
      paste('Drosophila (TAU) Network file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(dros_net, file, row.names = F, sep = "\t")
    }
  )
  output$dros_annot <- downloadHandler(
    filename = function() {
      paste('Drosophila KEGG pathways', '.txt', sep = '')
    },
    content = function(file) {
      write.table(dros_annot, file,row.names = F,col.names = F, sep = "\t")
    }
  )
  output$dros_louvain <- downloadHandler(
    filename = function() {
      paste('Drosophila Louvain automated annotation', '.txt', sep = '')
    },
    content = function(file) {
      write.table(dros_louvain, file,row.names = F,col.names = F, sep = "\t")
    }
  )
  output$dros_express <- downloadHandler(
    filename = function() {
      paste('Drosophila Expressions file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(dros_express, file,row.names = F,col.names = F, sep = "\t")
    }
  )
  
  #-------------------------------------------#
  
  string_net_tp53 <-
    read.delim("Examples/TP53/string_interactions.txt", header = T)
  string_annot <-
    read.delim("Examples/TP53/string_interactions_groups_comma_duplicate.txt",
               header = F)
  string_expr <-
    read.delim("Examples/TP53/string_expression_colors.txt", header = F)
  
  output$string_net_tp53 <- downloadHandler(
    filename = function() {
      paste('STRING TP53 Network file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(string_net_tp53, file, row.names = F, sep = "\t")
    }
  )
  output$string_annot <- downloadHandler(
    filename = function() {
      paste('STRING TP53 Annotation file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(string_annot, file, row.names = F,col.names = F, sep = "\t")
    }
  )
  output$string_expr <- downloadHandler(
    filename = function() {
      paste('STRING TP53 Expression file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(string_expr, file, row.names = F,col.names = F, sep = "\t")
    }
  )
  
  #-------------------------------------------#
  
  string_net_bcar3 <-
    read.delim("Examples/BCAR3/BCAR3.txt", header = T)
  string_bp <-
    read.delim("Examples/BCAR3/BCAR3_GO_BP.txt", header = F)
  string_mf <-
    read.delim("Examples/BCAR3/BCAR3_GO_MF.txt", header = F)
  string_kegg <-
    read.delim("Examples/BCAR3/BCAR3_KEGG.txt",
               header = F)
  
  output$string_net_bcar3 <- downloadHandler(
    filename = function() {
      paste('STRING BCAR3 Network file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(string_net_bcar3, file, row.names = F, sep = "\t")
    }
  )
  output$string_bp <- downloadHandler(
    filename = function() {
      paste('STRING BCAR3 GO Biological Processes file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(string_bp, file, row.names = F,col.names = F, sep = "\t")
    }
  )
  output$string_mf <- downloadHandler(
    filename = function() {
      paste('STRING BCAR3 GO Molecura functions file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(string_mf, file, row.names = F,col.names = F, sep = "\t")
    }
  ) 
  output$string_kegg <- downloadHandler(
    filename = function() {
      paste('STRING BCAR3 GO KEGG file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(string_kegg, file, row.names = F,col.names = F, sep = "\t")
    }
  )
  
  #-------------------------------------------#
  co_express <-
    read.delim("Examples/Human_Coexpression/NORMA_Human_coexpression_NETWORK.txt", header = T)
  co_express_bp <-
    read.delim("Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_GO_BP.txt", header = F)
  co_express_mf <-
    read.delim("Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_GO_MF.txt", header = F)
  co_express_cc <-
    read.delim("Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_GO_CC.txt",
               header = F)
  co_express_kegg <-
    read.delim("Examples/Human_Coexpression/NORMA_Human_coexpression_Annotation_KEGG.txt", header = F)
  co_express_mcode <-
    read.delim("Examples/Human_Coexpression/NORMA_Human_coexpression_Expression_MCODE.txt",
               header = F)
  
  output$co_express <- downloadHandler(
    filename = function() {
      paste('Human Gene Co-expression Network file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(co_express, file, row.names = F, sep = "\t")
    }
  )
  output$co_express_bp <- downloadHandler(
    filename = function() {
      paste('Human Gene Co-expression GO Biological Processes file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(co_express_bp, file,row.names = F,col.names = F, sep = "\t")
    }
  )
  output$co_express_mf <- downloadHandler(
    filename = function() {
      paste('Human Gene Co-expression GO Molecura functions file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(co_express_mf, file,row.names = F,col.names = F, sep = "\t")
    }
  ) 
  output$co_express_cc <- downloadHandler(
    filename = function() {
      paste('Human Gene Co-expression Cellular Componets file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(co_express_cc, file,row.names = F,col.names = F, sep = "\t")
    }
  )
  output$co_express_kegg <- downloadHandler(
    filename = function() {
      paste('Human Gene Co-expression KEGG pathways file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(co_express_kegg, file,row.names = F,col.names = F, sep = "\t")
    }
  ) 
  output$co_express_mcode <- downloadHandler(
    filename = function() {
      paste('Human Gene Co-expression MCODE node coloring file', '.txt', sep = '')
    },
    content = function(file) {
      write.table(co_express_mcode, file,row.names = F,col.names = F, sep = "\t")
    }
  )
  ##############################################################################
  R_script <- read.delim("annotation_cleaner.R", header = F)
  
  output$R_script <- downloadHandler(
    filename = function() {
      paste('Annotation_cleaner', '.R', sep = '')
    },
    content = function(file) {
       write.table(R_script, file, row.names = F,col.names = F, sep = "\t", quote = F)
    }
  ) 
  
  
  
  
})# The End