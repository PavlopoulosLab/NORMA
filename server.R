shinyServer(function(input, output, session) {
  # source function files
  source('./functions/upload.R', local=TRUE)
  source('./functions/network.R', local=TRUE)
  source('./functions/annotations.R', local=TRUE)
  source('./functions/topology.R', local=TRUE)
  source('./functions/helpDownloadHandlers.R', local=TRUE)
  
  set.seed(123)
  
  # ~~~GLOBAL VARS~~~ ####
  colors <- randomColor(50)
  area1<- NULL
  area2<- NULL
  v1<- NULL
  v2<- NULL
  
  # reactiveValues ####
  reactiveVars <- reactiveValues()
  reactiveVars$StoredNetworks <- data.frame(id = character(),
                                            name = character(),
                                            stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds <- c()
  reactiveVars$StoredAnnotations <- data.frame(id = character(),
                                               name = character(),
                                               stringsAsFactors = F)
  reactiveVars$SelectedStoredAnnotationIds <- c()
  reactiveVars$StoredExpressions <- data.frame(id = character(),
                                               name = character(),
                                               stringsAsFactors = F)
  reactiveVars$SelectedStoredExpressionIds <- c()
  reactiveVars$StoredNetworks_just_network <- data.frame(id = character(),
                                                         name = character(),
                                                         stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds_just_network <- c()
  ### Duplicated upload for selctInput boxes of networks in annotation-tab (_annotations_tab)###
  reactiveVars$StoredNetworks_annotations_tab <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds_annotations_tab <- c()
  ### Duplicated upload for annotations in annotation-tab ###
  reactiveVars$StoredNetworks2_annotations_tab <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds2_annotations_tab <- c()
  ### Topology tab ###
  reactiveVars$StoredNetworks_topology_tab <-
    data.frame(id = character(),
               name = character(),
               stringsAsFactors = F)
  reactiveVars$SelectedStoredNetworksIds_topology_tab <- c()
  
  # reactive ####
  StoredNets <- reactive({ return(reactiveVars$StoredNetworks) })
  StoredNets_annotations_tab <- reactive({ return(reactiveVars$StoredNetworks_annotations_tab) })
  StoredNets2_annotations_tab <- reactive({ return(reactiveVars$StoredNetworks2_annotations_tab) })
  StoredNets_topology_tab <- reactive({ return(reactiveVars$StoredNetworks_topology_tab) })
  StoredAnnots <- reactive({ return(reactiveVars$StoredAnnotations) })
  StoredExpress <- reactive({ return(reactiveVars$StoredExpressions) })
  fetchFirstSelectedStoredDataset <- reactive({
    ssn <- SelectedStoredNets()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      return(fetchDataset(ssn[1,]$id))
    } else {
      return(NULL)
    }
  })
  fetchFirstSelectedStoredDataset2 <- reactive({
    sannots <- SelectedStoredAnnots()
    if (!is.null(sannots) && nrow(sannots) > 0) {
      return(fetchDataset(sannots[1,]$id))
    } else {
      return(NULL)
    }
  })
  fetchFirstSelectedStoredDataset_annotations_tab <- reactive({
    ssn <- SelectedStoredNets_annotations_tab()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      return(fetchDataset_annotations_tab(ssn[1,]$id))
    } else {
      return(NULL)
    }
  })
  fetchFirstSelectedStoredGroups2_annotations_tab <- reactive({
    sannots <- SelectedStoredNets2_annotations_tab()
    if (!is.null(sannots) && nrow(sannots) > 0) {
      return(fetchDataset2_annotations_tab(sannots[1,]$id))
    } else {
      return(NULL)
    }
  })
  fetchFirstSelectedStoredExpression <- reactive({
    sexpress <- SelectedStoredExpress()
    if (!is.null(sexpress) && nrow(sexpress) > 0) {
      return(fetchDatasetEx(sexpress[1,]$id))
    } else {
      return(NULL)
    }
  })
  fetchFirstSelectedStoredDataset_topology_tab <- reactive({
    ssn <- SelectedStoredNets_topology_tab()
    if (!is.null(ssn) && nrow(ssn) > 0) {
      return(fetchDataset_topology_tab(ssn[1,]$id))
    } else {
      return(NULL)
    }
  })
  datasetInput <- reactive({
    automated_annotations <- input$automated_annotations
    
    source("./functions/refreshing/modularity.R", local = T)
    net <- fetchFirstSelectedStoredIgraph_just_network()
    if (is.null(net))
      return()
    modularity()
  })
  # Scaling - Sliders
  scaling_coordinates_convex <- reactive({ input$scaling_coordinates_convex })
  scaling_coordinates_pies <- reactive({ input$scaling_coordinates_pies })
  scaling_nodes_convex <- reactive({ input$scaling_nodes_convex })
  scaling_nodes_pies <- reactive({ input$scaling_nodes_pies })
  scaling_labels_convex <- reactive({ input$scaling_labels_convex })
  scaling_labels_pies <- reactive({ input$scaling_labels_pies })
  scaling_coordinates_convex_3D_X <- reactive({ input$scaling_coordinates_convex_3D_X }) 
  scaling_coordinates_convex_3D_Y <- reactive({ input$scaling_coordinates_convex_3D_Y }) 
  scaling_coordinates_convex_3D_Z <- reactive({ input$scaling_coordinates_convex_3D_Z })
  
  # ~~~UPLOAD~~~ ####
  # ~~Upload sidebar~~ ####
  # observeEvents ####
  doAddNetwork <- observeEvent(input$btnAddNetwork, {
    tryCatch({
      addNetwork()
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  doRemNetwork <- observeEvent(input$btnRemoveNetworks, {
    tryCatch({
      remNetwork()
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  doAddAnnotations <- observeEvent(input$btnAddNetwork2, {
    tryCatch({
      addAnnotations()
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  doRemAnnotations <- observeEvent(input$btnRemoveNetworks2, {
    tryCatch({
      remAnnotations()
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  doAddExpression <- observeEvent(input$btnAddExpression, {
    tryCatch({
      addExpression()
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  doRemExpression <- observeEvent(input$btnRemoveExpression, {
    tryCatch({
      remExpression()
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  uploadTabSetSelectedNetwork <- observeEvent(input$storedGraphsOutputSelectUpload, {
    tryCatch({
      reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputSelectUpload)
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  }, ignoreNULL = FALSE)
  
  # Add-Annotations button 
  uploadTabSetSelectedNetwork2 <- observeEvent(input$storedGraphsOutputSelectUpload2, {
    tryCatch({
      reactiveVars$SelectedStoredAnnotationIds <- c(input$storedGraphsOutputSelectUpload2)
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  }, ignoreNULL = FALSE)
  
  # observes ####
  # Change of Network Name based on input choices
  dochangeNetworkName <- observe({
    tryCatch({
      updateTextInput(session,
                      inputId = "networkName",
                      value = (if (input$uiLoadGraphOptionsInput == "OF") {
                        paste("Network name")
                      } else if (input$uiLoadGraphOptionsInput == "oR_String_interactions"){
                        paste("BCAR3 STRING Network")
                      } else if (input$uiLoadGraphOptionsInput == "oR_Drosophila"){
                        paste("Drosophila TAU Network")
                      }))
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  # Change Annotation Name based on input choices
  dochangeAnnotationName <- observe({
    tryCatch({
      updateTextInput(session,
                      inputId = "annotationName",
                      value = (if (input$uiLoadGraphOptionsInput_annotations == "OF") {
                        paste("Annotation name")
                      } else if (input$uiLoadGraphOptionsInput_annotations == "oR_String_Annotation_BP"){
                        paste("BCAR3 GO Biological Process")
                      } else if (input$uiLoadGraphOptionsInput_annotations == "oR_String_Annotation_MF"){
                        paste("BCAR3 GO Molecular Function")
                      } else if (input$uiLoadGraphOptionsInput_annotations == "oR_String_Annotation_KEGG"){
                        paste("BCAR3 GO KEGG pathways")
                      } else if (input$uiLoadGraphOptionsInput_annotations == "oR_Drosophila_KEGG"){
                        paste("Drosophila TAU KEGG pathways")
                      } else if (input$uiLoadGraphOptionsInput_annotations == "oR_Drosophila_Luvain"){
                        paste("Drosophila TAU Louvain")
                      } else if (input$uiLoadExpressionsInput == "oR_Expression_file_Drosophila"){
                        paste("Drosophila TAU node - coloring file")
                      }))
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  # UI render outputs ####
  output$uiLoadGraphOptionsOutput <- renderUI({
    tryCatch({
      if (is.null(input$uiLoadGraphOptionsInput)) return()
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
          ))#,
          #div(list(div(wellPanel(checkboxInput(inputId = "weighted1", label = "Weighted", value = F)), class = "col-md-6")), class = "row")
        )
      } 
      # else {
      #   div(list(div(wellPanel(checkboxInput(inputId = "weighted1", label = "Weighted", value = F)), class = "col-md-6")), class = "row")
      # }
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  output$uiStoredGraphsOutputRadio <- renderUI({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  output$uiLoadGraphOptionsOutput_annotations <- renderUI({
    tryCatch({
      if (is.null(input$uiLoadGraphOptionsInput_annotations)) return()
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
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  output$uiLoadExpressionsOutput <- renderUI({
    tryCatch({
      if (is.null(input$uiLoadExpressionsInput)) return()
      # Depending on input$input_type, we'll generate a different UI
      # component and send it to the client.
      if (input$uiLoadExpressionsInput == "oF") {
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
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  output$uiStoredGraphsOutputRadioEx <- renderUI({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  # ~~Upload mainPanel~~ ####
  # UI render outputs ####
  
  # select box for network edgelists
  output$uiStoredGraphsOutputSelectUpload <- renderUI({
    tryCatch({
      input$btnAddNetwork
      input$btnRemoveNetworks
      choices <- getStoredNetsChoices()
      if (is.null(choices)) return()
      return(selectInput(
        "storedGraphsOutputSelectUpload",
        "Selected network",
        choices
      ))
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  # select box for annotations
  output$uiStoredGraphsOutputSelectUpload2 <- renderUI({
    tryCatch({
      input$btnAddNetwork2
      input$btnRemoveNetworks2
      choices <- getStoredAnnotChoices()
      if (is.null(choices)) return()
      return(
        selectInput(
          "storedGraphsOutputSelectUpload2",
          "Selected annotation",
          choices
        )
      )
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  # Table format - No loops allowed (simplify)
  output$datasettab1 <- DT::renderDataTable({
    tryCatch({
      dataset <- fetchFirstSelectedStoredDataset()
      if (is.null(dataset))
        dataset <- EmptyDataset(c("Source", "Target", "Weight"))
      if (nrow(dataset) == 0 || attr(dataset, 'weighted')){
        return(datatable(dataset, rownames = F, editable = F, options=list(deferRender = TRUE, scrollY = 500, scroller = TRUE,pageLength = 500)
        ) %>%
          formatStyle(colnames(dataset), fontSize = ui_options["ui_table_font_sz"]) %>%
          formatRound("Weight")
        )
      } else {
        return(datatable(dataset, rownames = FALSE, options=list(deferRender = TRUE,
                                                                 scrollY = 500,
                                                                 scroller = TRUE,pageLength = 500,
                                                                 columnDefs = list(list(visible=FALSE, targets=c(2))))) %>%
                 formatStyle(colnames(dataset), fontSize = ui_options["ui_table_font_sz"]) %>%
                 formatRound("Weight"))
      }
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  # Table view of Annotations
  output$datasettab2 <- DT::renderDataTable({
    tryCatch({
      annotation <- fetchFirstSelectedStoredDataset2()
      if (is.null(annotation)) annotation <- EmptyDataset(c("Annotations", "Nodes"))
      annotation$Nodes <- gsub(',', ', ', annotation$Nodes)
      datatable(annotation, rownames = FALSE, extensions = 'Responsive') %>%
        formatStyle(colnames(annotation), fontSize = ui_options["ui_table_font_sz"])
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  output$uiStoredGraphsOutputRadio_annotations <- renderUI({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Upload tab error: ", e))
      shinyalert("Error!", "Upload tab error.", type = "error")
    })
  })
  
  # ~~~NETWORK ~~~ ####
  # ObserveEvents ####
  uploadTabSetSelectedNetwork_just_network <- observeEvent(input$storedGraphsOutputSelectUpload_just_network, {
    tryCatch({
      reactiveVars$SelectedStoredNetworksIds_just_network <- c(input$storedGraphsOutputSelectUpload_just_network)
    }, error = function(e) {
      print(paste("Network tab error: ", e))
      shinyalert("Error!", "Network tab error.", type = "error")
    })
  }, ignoreNULL = FALSE)
  
  # UI render outputs ####
  # Duplicated upload for networks in network-tab (just_network)
  output$uiLoadGraphOptionsOutput_just_network <-  renderUI({
    tryCatch({
      input$btnAddNetwork
      input$btnRemoveNetworks
      choices <- getStoredNetsChoices_just_network()
      if (is.null(choices)) return()
      return( selectInput(
        "storedGraphsOutputSelectUpload_just_network",
        "Selected network", choices)
      )
    }, error = function(e) {
      print(paste("Network tab error: ", e))
      shinyalert("Error!", "Network tab error.", type = "error")
    })
  })
  
  # Interactive visNetwork
  output$tabVizIgraphSimple <- renderVisNetwork({
    tryCatch({
      set.seed(123)
      g <- fetchFirstSelectedStoredIgraph_just_network()
      if (is.null(g))
        return()
      igraph_visn <- toVisNetworkData(g)
      igraph_visn$edges$value <- E(g)$weight
      
      set.seed(123)
      if(is.weighted(g)){
        withProgress(min = 0, max = 1, {
          incProgress(message = "Plotting",
                      detail = "This may take a while...",
                      amount = .1)
          set.seed(123)
          visNetwork(igraph_visn$nodes, igraph_visn$edges) %>%
            visNodes(size = 25, shape = "ellipse") %>%
            visEdges(smooth = FALSE) %>%
            visIgraphLayout(layout = "layout_with_fr") %>%
            visOptions(highlightNearest = TRUE,
                       nodesIdSelection = TRUE) %>%
            visPhysics(enabled = F) %>%
            visInteraction(
              keyboard = TRUE,
              navigationButtons = TRUE,
              zoomView = TRUE,
              multiselect = TRUE,
              dragView = TRUE
            )
        })
      } else{
        withProgress(min = 0, max = 1, {
          incProgress(message = "Plotting",
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
      }
    }, error = function(e) {
      print(paste("Network tab error: ", e))
      shinyalert("Error!", "Network tab error.", type = "error")
    })
  })
  
  output$modularity_plot <- renderPlot({
    tryCatch({
      # show_labels_algorithms_tab <- T
      automated_annotations <- input$automated_annotations
      net <- fetchFirstSelectedStoredIgraph_just_network()
      if (is.null(net))
        return()
      clp <-
        automated_annotation_choices(net, automated_annotations)
      
      if (input$show_labels_algorithms_tab == F) {
        # show_labels_algorithms_tab = F
        set.seed(123)
        withProgress(min = 0, max = 1, {
          incProgress(message = "Plotting",
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
          incProgress(message = "Plotting",
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
    }, error = function(e) {
      print(paste("Network tab error: ", e))
      shinyalert("Error!", "Network tab error.", type = "error")
    })
  })
  
  output$Modularity_table <- DT::renderDataTable({
    tryCatch({
      net <- fetchFirstSelectedStoredIgraph_just_network()
      if (is.null(net)) {
        df <- EmptyDataset(c("Annotations", "Nodes"))
      }
      else{
        automated_annotations <- input$automated_annotations
        source("./functions/refreshing/modularity.R", local = T)
        df <- modularity()
        datatable(
          df,
          colnames = c("Annotations", "Nodes"),
          rownames = FALSE,
          extensions = 'Responsive'
        ) %>% formatStyle(colnames(df), fontSize = ui_options["ui_table_font_sz"])
      }
    }, error = function(e) {
      print(paste("Network tab error: ", e))
      shinyalert("Error!", "Network tab error.", type = "error")
    })
  })
  
  # downloadHandlers ####
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
  
  # ~~~ANNOTATIONS~~~ ####
  # observeEvents ####
  uploadTabSetSelectedNetwork_annotations_tab <- observeEvent(input$storedGraphsOutputSelectUpload_annotations_tab, {
    tryCatch({
      reactiveVars$SelectedStoredNetworksIds_annotations_tab <- c(input$storedGraphsOutputSelectUpload_annotations_tab)
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  }, ignoreNULL = FALSE)
  
  uploadTabSetSelectedNetwork2_annotations_tab <- observeEvent(input$storedGraphsOutputSelectUpload2_annotations_tab, {
    tryCatch({
      reactiveVars$SelectedStoredNetworksIds2_annotations_tab <- c(input$storedGraphsOutputSelectUpload2_annotations_tab)
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  }, ignoreNULL = FALSE)
  
  uploadTabSetSelectedExpression <- observeEvent(input$uiStoredGraphsOutputSelectUploadExpressions, {
    tryCatch({
      reactiveVars$SelectedStoredExpressionIds <- c(input$uiStoredGraphsOutputSelectUploadExpressions)
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  }, ignoreNULL = FALSE)
  
  # observes ####
  node1_choice<- observe({
    tryCatch({
      node1 <- input$node_1
      node2 <- input$node_2
      
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
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  node2_choice<- observe({
    tryCatch({
      node2 <- input$node_2
      node1 <- input$node_1
      
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
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  # UI render outputs ####
  # 3 conditional panels
  output$uiLoadGraphOptionsOutput_annotations_tab <- renderUI({
    tryCatch({
      input$btnAddNetwork
      input$btnRemoveNetworks
      choices <- getStoredNetsChoices_annotations_tab()
      if (is.null(choices)) return()
      return(
        selectInput(
          "storedGraphsOutputSelectUpload_annotations_tab",
          "Selected network",
          choices
        )
      )
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  output$uiLoadGraphOptionsOutput_annotations_annotations_tab <-  renderUI({
    tryCatch({
      input$btnAddNetwork2
      input$btnRemoveNetworks2
      choices <- getStoredNetsChoices2_annotations_tab()
      if (is.null(choices)) return()
      return(
        selectInput(
          "storedGraphsOutputSelectUpload2_annotations_tab",
          "Selected annotation",
          choices
        )
      )
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  output$uiStoredGraphsOutputSelectUploadExpressions <- renderUI({
    tryCatch({
      input$btnAddExpression
      input$btnRemoveExpression
      choices <- getStoredExpressionChoices()
      if (is.null(choices)) return()
      return(
        selectInput(
          "uiStoredGraphsOutputSelectUploadExpressions",
          "Selected Expression",
          choices
        )
      )
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  # Convex hulls
  output$interactive_convex_hulls <- renderUI({
    tryCatch({
      g <- fetchFirstSelectedStoredIgraph_annotations_tab()
      annotation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
      if (is.null(g) | is.null(annotation_graph)) return(NULL)
      
      withProgress(min = 0, max = 1, {
        incProgress(message = "Plotting",
                    detail = "This may take a while...",
                    amount = .1)
        
        start_time <- Sys.time()
        convex_hulls(g, annotation_graph)
        end_time <- Sys.time()
        if (PRINT_TIMES) print(paste("### TIME:", input$convex_layout_strategy, ": ", end_time - start_time, " seconds."))
        
        tags$iframe(
          srcdoc = paste(readLines(
            paste(USER_TEMP_FOLDER, "/output_convex_", session$token, ".html", sep = "")
          ), collapse = '\n'),
          width = "100%",
          height = "850px"
        )
      })
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  output$chooseGroups <- DT::renderDataTable({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  # PieCharts
  output$tabVizPie_charts <- renderUI({
    tryCatch({
      g <- fetchFirstSelectedStoredIgraph_annotations_tab()
      annotation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
      if (is.null(g) | is.null(annotation_graph)) return(NULL)
      
      withProgress(min = 0, max = 1, {
        incProgress(message = "Plotting",
                    detail = "This may take a while...",
                    amount = .1)
        
        pie_charts(g, annotation_graph)
        tags$iframe(
          srcdoc = paste(readLines(
            paste(USER_TEMP_FOLDER, "/output_pies_", session$token, ".html", sep = "")
          ), collapse = '\n'),
          width = "100%",
          height = "850px"
        )
      })
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  # PieCharts 
  output$chooseGroups2 <- DT::renderDataTable({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  # Convex hulls 3D
  output$convex_hull_3D<- renderUI({
    tryCatch({
      s = input$chooseGroups_3D_rows_selected
      
      g <- fetchFirstSelectedStoredIgraph_annotations_tab()
      annotation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
      if (is.null(g) | is.null(annotation_graph))
        return(NULL)
      
      if (input$Dark == T) {
        Dark_mode = T
      }
      else  if (input$Dark == F) {
        Dark_mode = F
      }
      if (input$show_labels_3D == T) {
        show_labels_3D = T
      }
      else  if (input$show_labels_3D == F) {
        show_labels_3D = F
      }
      if (input$expressions_3D == T) {
        expression_colors_3D = T
      }
      else if (input$expressions_3D == F) {
        expression_colors_3D = F
      }
      # if (input$layouts_with_virtual_nodes_3D == T) {
      #   layouts_with_virtual_nodes_3D = T
      # }
      # else  if (input$layouts_with_virtual_nodes_3D == F) {
      #   layouts_with_virtual_nodes_3D = F
      # }
      if (input$show_some_labels_3D == T) {
        show_some_labels_3D = T
      }
      else  if (input$show_some_labels_3D == F) {
        show_some_labels_3D = F
      }
      
      withProgress(min = 0, max = 1, {
        incProgress(message = "Plotting",
                    detail = "This may take a while...",
                    amount = .1)
        
        lay <- input$layouts_3D
        source("./functions/refreshing/convex_hulls_3D.R", local = T)
        convex_hull_3D()
        
        tags$iframe(
          srcdoc = paste(readLines(
            paste(USER_TEMP_FOLDER, "/convex_3D_", session$token,".html", sep="")
          ), collapse = '\n'),
          width = "100%",
          height = "850px"
        )
      })
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  output$chooseGroups_3D <- DT::renderDataTable({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  # Venn Diagrams
  output$vennDiagram1<- renderUI({
    tryCatch({
      venn <- vennDiagrams()
      selectInput("node_1", label = "Node 1", choices = c("-",venn$id), selected = "-")
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  output$vennDiagram2<- renderUI({
    tryCatch({
      venn<-vennDiagrams()
      selectInput("node_2", label = "Node 2", choices = c("-",venn$id), selected = "-")
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  output$vennDiagrams <- renderPlot({
    tryCatch({
      node1_choice<- observe({ node1 <- input$node_1 })
      
      if(!is.null(input$node_1)){
        venn <- vennDiagrams()
        for(i in length(venn$id)){
          venn_i<-which(venn$id==input$node_1)
          v1 <- unlist(stri_split(venn[venn_i,2],fixed=','))
          area1<-length(v1)
        }
      }
      
      node2_choice<- observe({ node2 <- input$node_2 })
      
      if(!is.null(input$node_2)){
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
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  output$venn_table_summ <- DT::renderDataTable({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Annotations tab error: ", e))
      shinyalert("Error!", "Annotations tab error.", type = "error")
    })
  })
  
  # downloadHandlers ####
  output$HTML_convex <- downloadHandler(
    filename = function() {
      paste("Convex_hulls_2D_", session$token, ".html", sep = "")
    },
    content = function(file) {
      HTML_convex <- paste(readLines(paste(USER_TEMP_FOLDER, "/output_convex_", session$token, ".html", sep = "")
      ), collapse = '\n')
      write.table(HTML_convex, file, row.names = F,col.names = F, sep = "\t", quote = F)
    }
  )
  
  output$HTML_pies <- downloadHandler(
    filename = function() {
      paste("Pie_charts_", session$token, ".html", sep = "")
    },
    content = function(file) {
      HTML_pies <- paste(readLines(
        paste(USER_TEMP_FOLDER, "/output_pies_", session$token, ".html", sep = "")
      ), collapse = '\n')
      write.table(HTML_pies, file, row.names = F,col.names = F, sep = "\t", quote = F)
    }
  )
  
  output$HTML_convex_3D <- downloadHandler(
    filename = function() {
      paste("Convex_hulls_3D_", session$token, ".html", sep = "")
    },
    content = function(file) {
      HTML_convex_3D <- paste(readLines(
        paste(USER_TEMP_FOLDER, "/convex_3D_", session$token, ".html", sep="")
      ), collapse = '\n')
      write.table(HTML_convex_3D, file, row.names = F,col.names = F, sep = "\t", quote = F)
    }
  )
  
  # ~~~TOPOLOGY~~~ ####
  # ~~Topology sidebar~~ ####
  # UI render outputs ####
  output$uiStoredGraphsOutputSelectTopolopgy <- renderUI({
    tryCatch({
      input$btnAddNetwork
      input$btnRemoveNetworks
      choices <- getStoredNetsChoices_topology_tab()
      if (is.null(choices)) return()
      return(selectInput(
        "storedGraphsOutputSelectTopolopgy",
        "Selected network",
        choices
      ))
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  output$uiStoredGraphsOutputMultipleSelectTopolopgy <- renderUI({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  # ~~Topology mainPanel~~ ####
  # observeEvents ####
  topologyTableViewSetSelectedNetwork <- observeEvent({
    input$storedGraphsOutputSelectTopolopgy
    input$statisticsMethodsMainTabsetPanel
  }, {
    tryCatch({
      if (input$statisticsMethodsMainTabsetPanel == "tableView" &&
          !(
            length(reactiveVars$SelectedStoredNetworksIds_topology_tab) ==
            1 &&
            reactiveVars$SelectedStoredNetworksIds_topology_tab == input$storedGraphsOutputSelectTopolopgy
          )) {
        reactiveVars$SelectedStoredNetworksIds_topology_tab <-
          c(input$storedGraphsOutputSelectTopolopgy)
      }
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  }, ignoreNULL = FALSE)
    
  topologyPlotViewSetSelectedNetwork <- observeEvent({
    input$storedGraphsOutputMultipleSelectTopolopgy
    input$statisticsMethodsMainTabsetPanel}, {
      tryCatch({
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
      }, error = function(e) {
        print(paste("Topology tab error: ", e))
        shinyalert("Error!", "Topology tab error.", type = "error")
      })
    }, ignoreNULL = FALSE)
  
  doStatSelectAll <- observeEvent(input$btnStatSelectAll, {
    tryCatch({
      updateCheckboxGroupInput(session, "statistics2", selected = statistics)
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  doStatSelectNone <- observeEvent(input$btnStatSelectNone, {
    tryCatch({
      updateCheckboxGroupInput(session, "statistics2", selected = character(0))
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  doRefreshPalette <- observeEvent(input$btnRefreshPalette, {
    tryCatch({
      ncolors <- length(colors)
      colors <<- randomColor(ncolors)
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  # UI render outputs ####
  output$statisticsMethodsPlotRender <- renderUI({
    tryCatch({
      plotOutput("statisticsMethodsBarPlot", height = paste0(as.integer(
        480 +
          480 * (input$statisticsPlotPercentMagnify / 100)
      ), "px"))
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  output$statisticsMethodsBarPlot <- renderPlot({
    tryCatch({
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
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  output$statres <- DT::renderDataTable({
    tryCatch({
      dset <- fetchFirstSelectedStoredDataset_topology_tab()
      DT::datatable(
        stat_dataset2(dset, getDatasetName(attr(dset, "id"))),
        options = list(
          paging = FALSE,
          dom = "t",
          rowCallback = JS(
            "function(row, data) {",
            "var num = parseFloat(data[2]).toFixed(2);",
            "if(isNaN(num)){num = data[2];}",
            "$('td:eq(2)', row).html(num);",
            "}"
          )
        )
      )
    }, error = function(e) {
      print(paste("Topology tab error: ", e))
      shinyalert("Error!", "Topology tab error.", type = "error")
    })
  })
  
  # onSessionEnded ####
  session$onSessionEnded(function() {
    snets <- isolate(StoredNets())
    if (nrow(snets) > 0) {
      unlink(c(paste0(USER_TEMP_FOLDER, "/", "*.rda"), "*.zip"))
    }
    
    sannots <- isolate(StoredAnnots())
    if (nrow(sannots) > 0) {
      unlink(c(paste0(USER_TEMP_FOLDER, "/", "*.rda"), "*.zip"))
    }
    
    sexpress <- isolate(StoredExpress())
    if (nrow(sexpress) > 0) {
      unlink(c(paste0(USER_TEMP_FOLDER, "/", "*.rda"), "*.zip"))
    }
  })
  
})
