convex_hull_3D <- function() {
  set.seed(123)
  
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g))
    return()
  
  # dataset <-  get.edgelist(g)
  dataset <-  fetchFirstSelectedStoredDataset_annotations_tab()
  if (is.null(dataset))
    return()
  
  original_dataset_weighted <- fetchFirstSelectedStoredDataset_annotations_tab()
  if (is.null(original_dataset_weighted))
    return(NULL)
  
  annotation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  if (is.null(annotation_graph))
    return()
  
  #---------------------------------------------------------------#
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(Source = my_network$V1, Target = my_network$V2)
  
  groups <- as.data.frame(annotation_graph)

  groups<- data.frame(V1 = groups$Annotations, stri_split_fixed(groups$Nodes, ",",  simplify = TRUE))
  groups<-mutate_all(groups, funs(na_if(.,"")))
  number_of_groups<-dim(groups)[1]
  
  x <- list()
  for (i in 1:number_of_groups) {
    group_i<- groups[i,]
    group_i<- group_i[,-1]
    group_i <- group_i[!is.na(group_i)]
    x[[i]]<- (group_i)
  }
  
  GO <- list()
  for (i in 1:number_of_groups) {
    GO[[i]]<-rep(groups[i,1], length(x[[i]]))
  }
  
  column1<-my_network$Source
  column2<-my_network$Target
  node_names<-unique(union(column1, column2))
  tt<-unlist(x)
  nodes_with_NA_groups<-setdiff(node_names,tt)
  
  members <- data_frame(id=unlist(x),group = unlist(GO))
  members_with_NA_groups <- data_frame(id=unlist(x),group = unlist(GO))
  #----------------------------------------------------------------#
  
  #-------------------------------------------------------------------------#
  
  new_nodes <- unique(union(dataset[,1], dataset[,2]))
  
  new_annot <- annotation_graph %>% 
    mutate(V2 = strsplit(as.character(annotation_graph$Nodes), ",")) %>% 
    unnest(V2)
  
  intersect_g_annot <- intersect(new_nodes, new_annot$V2)
  
  if(identical(intersect_g_annot, character(0))){
    showModal(modalDialog(
      title = "Important message",
      "Please check if the selected annotation file corresponds to the selected network.",
      easyClose = T
    ))
  }
  #-------------------------------------------------------------------------#

    set.seed(123)
    lay <- layout_choices_3D(g, lay)

  #----------------------------------------------------------------#
  scene_scale_x_max <- max(lay[,1])*scaling_coordinates_convex_3D_X()
  scene_scale_x_min <- min(lay[,1])*scaling_coordinates_convex_3D_X()
  
  scene_scale_y_max <- max(lay[,2])*scaling_coordinates_convex_3D_Y()
  scene_scale_y_min <- min(lay[,2])*scaling_coordinates_convex_3D_Y()
  
  scene_scale_z_max <- max(lay[,3])*scaling_coordinates_convex_3D_Z()
  scene_scale_z_min <- min(lay[,3])*scaling_coordinates_convex_3D_Z()
  #----------------------------------------------------------------#
  # lay <- lay*scaling_coordinates_convex_3D()
  #-- Scaling coordinates --#
  coorx<- lay[,1]*scaling_coordinates_convex_3D_X()
  lay <- cbind(coorx,lay[,2:ncol(lay)])

  coory<- lay[,2]*scaling_coordinates_convex_3D_Y()
  lay <- cbind(lay[,1], coory,lay[,3])

  coorz<- lay[,3]*scaling_coordinates_convex_3D_Z()
  lay <- cbind(lay[,1:2], coorz)
  #-------------------------#
  
  colnames(lay) <- c("x", "y", "z")
  # node_names_3D <- unique(union(dataset[,1], dataset[,2]))
  
  
  #---------------------------------#
 
    node_names_3D <- names(V(g))
    node_name_links_3D <- names(V(g))
  #--------------------------------#
  
  
  if(length(node_names_3D) != nrow(lay)){
    showModal(modalDialog(
      title = "Important message",
      "Please check if the selected annotation file corresponds to the selected network.",
      easyClose = T
    ))
  }
  else{
  node_names_with_coords <- data.frame("Source" = node_names_3D, 
                                       "x" = lay[,1], 
                                       "y" = lay[,2], 
                                       "z" = lay[,3])
  }
  
  
  #--- hash table for names and coordinates ---#
  coordinates_hashmap <- new.env(hash = TRUE)
  for(i in 1:nrow(node_names_with_coords)){
    coordinates_hashmap[[as.character(node_names_with_coords[i,1])]]<-c(node_names_with_coords[i,2], node_names_with_coords[i,3], node_names_with_coords[i,4])
  }
  
  #---------------------------------------------------#
  
  nrowdat <- nrow(dataset)
  nrowannot <- nrow(annotation_graph)
  
  
  if(!(is.weighted(g))){
    original_dataset_weighted <- cbind(original_dataset_weighted[,1:2],"Weight"=rep(0.5, nrow(original_dataset_weighted)))
  } else{
    minx <- min(original_dataset_weighted[,3])
    maxx <- max(original_dataset_weighted[,3])
    
    scaling_weight_values<- c()
    for (i in 1:nrow(original_dataset_weighted)){
      scaling_weight_values_i <- mapper(original_dataset_weighted[i,3], minx, maxx, 0.5, 20)
      scaling_weight_values <- c(scaling_weight_values, scaling_weight_values_i)
    }
    if(maxx > 50){
    original_dataset_weighted <- cbind(original_dataset_weighted[,1:2],"Weight"=scaling_weight_values)
    }
  }

  if (length(s)==0)
  {
    s<-c(1:nrowannot)
  }
  
  if (length(s)) {
    s<-sort(s)#-----------------------------------
    x<- length(s)
    
    dataset1 <- get.edgelist(g)
    dataset1 <- data.frame("Source" = dataset[,1], "Target" = dataset[,2])
    
    source <- matrix(ncol=3, nrow=nrow(dataset1))
    target <- matrix(ncol=3, nrow=nrow(dataset1))
    for(i in 1:nrow(dataset1)){
      source[i,] <- coordinates_hashmap[[as.character(dataset1[i,1])]]
      target[i,] <- coordinates_hashmap[[as.character(dataset1[i,2])]]
    }

    edges_for_plotly <- data.frame("Source.x" = source[,1], "Source.y" = source[,2], "Source.z" = source[,3],
                                   "Target.x" = target[,1], "Target.y" = target[,2], "Target.z" = target[,3])
  # x<- c()
  # for(i in 1:nrow(edges_for_plotly)){
  #   if(i == nrow(edges_for_plotly)){
  #     a <- paste(edges_for_plotly$Source.x[i], edges_for_plotly$Target.x[i], "null", sep = "," )
  #   }else{
  #     a <- paste(edges_for_plotly$Source.x[i], edges_for_plotly$Target.x[i], "null,", sep = "," )
  # 
  #   }
  #   x<- c(x, a)
  # }
  # 
  # x <- as.vector(unlist(x))
  # x <- paste(x,  collapse=" ")
  # 
  # y<- c()
  # for(i in 1:nrow(edges_for_plotly)){
  #   if(i == nrow(edges_for_plotly)){
  #     a = paste(edges_for_plotly$Source.y[i], edges_for_plotly$Target.y[i], "null", sep = "," )
  #   }else{
  #     a = paste(edges_for_plotly$Source.y[i], edges_for_plotly$Target.y[i], "null,", sep = "," )
  #   }
  #   y<- c(y, a)
  # }
  # 
  # y <- as.vector(unlist(y))
  # y <- paste(y,  collapse=" ")
  # 
  # 
  # z<- c()
  # for(i in 1:nrow(edges_for_plotly)){
  #   if(i == nrow(edges_for_plotly)){
  #     a = paste(edges_for_plotly$Source.z[i], edges_for_plotly$Target.z[i], "null", sep = "," )
  #   }else{
  #     a = paste(edges_for_plotly$Source.z[i], edges_for_plotly$Target.z[i], "null,", sep = "," )
  #   }
  #   z<- c(z, a)
  # }
  # 
  # z <- as.vector(unlist(z))
  # z <- paste(z,  collapse=" ")
  
  #--------------------------------------------------------------------#  
  fileConn <- file(paste(USER_TEMP_FOLDER, "/convex_3D_", session$token,".html", sep=""), "w")
  #--------------------------------------------------------------------#
  
  cat(sprintf("<!DOCTYPE html>
  <head>
     <script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>
   </head>
   <body>
   <!-- Plotly chart will be drawn inside this div -->
      <div id='plotly-div' div>

     <script>" ), file = fileConn)
  
  # cat(sprintf("
  # 
  # trace_edges = {
  # uid: 'bcd52d', 
  # line: {
  #   color: 'rgb(125,125,125)', 
  #   width: 0.5
  # }, 
  # mode: 'lines', 
  # name: 'Edges', 
  # type: 'scatter3d',"), file = fileConn)
  
 #---------------------------------------#   
  for(i in 1:nrow(edges_for_plotly)){
    x_trial <- paste(edges_for_plotly$Source.x[i], edges_for_plotly$Target.x[i], sep = "," )
    
      cat(sprintf(paste("trace_edges_",i," = {
  uid: 'bcd52d', 
  line: {
    color: 'rgb(125,125,125)', 
    width:", original_dataset_weighted[i,3], "
  }, 
  mode: 'lines', 
  name: 'Edges', 
  type: 'scatter3d',
                        x : [", edges_for_plotly$Source.x[i], ",", edges_for_plotly$Target.x[i], "],\n
                        y : [", edges_for_plotly$Source.y[i], ",", edges_for_plotly$Target.y[i], "],\n
                        z : [", edges_for_plotly$Source.z[i], "," ,edges_for_plotly$Target.z[i], "], \n
    hoverinfo : 'none'};
                        
                        " , sep="")), file = fileConn)
    
  }
  
  traces_edges<- c()
  for(i in 1:nrow(original_dataset_weighted)){
    traces_edges_i <- paste("trace_edges_", i, sep = "")
    traces_edges<- c(traces_edges,traces_edges_i)
    traces_edges <- paste(traces_edges, collapse=",")
  }
    
    #--- Nodes ---#
    
    x_nodes <- paste(lay[,1],  collapse=",")
    y_nodes <- paste(lay[,2],  collapse=",")
    z_nodes <- paste(lay[,3],  collapse=",")

    if(show_labels_3D == T){    
      show_labels <- "markers+text"
    }else{
      show_labels <- "markers"
    }
    
    if(Dark_mode ==T){
      textColor <-  "textfont: {
        color: 'white'
      },"
    } else{
      textColor <-  "textfont: {
        color: 'black'
      },"
    }
    cat(sprintf(paste("
  trace_nodes = {
  uid: 'a2e4a0', 
  mode: '", show_labels , "', 
  name: 'Nodes',", textColor,"
  type: 'scatter3d',", sep="")), file = fileConn)
    
    
    
    write(paste("
      x : [", x_nodes, "],\n" , sep=""), file = fileConn, append = T)
    write(paste("
      y : [", y_nodes, "],\n" , sep=""), file = fileConn, append = T)
    write(paste("
      z : [", z_nodes, "],\n" , sep=""), file = fileConn, append = T)
    

    #---- expression_colors_3D ----#
    if (!is.null(getStoredExpressionChoices())){
      expression<-fetchFirstSelectedStoredExpression()
      colnames(expression) <- c("id", "color")
      express_order<- as.data.frame(names(V(g)))
      colnames(express_order) <- "id"
      expression <- suppressMessages(left_join(express_order, expression, by = "id"))
      expression$color<- as.character(expression$color)
      for(i in 1:length(expression$color)){
        if(Dark_mode ==F){
          if(expression$color[i] == "" || is.na(expression$color[i])){
            expression$color[i] <- "purple"
          }
        } else if (expression$color[i] == "" || is.na(expression$color[i])){
          expression$color[i] <- "#a419bc"
        }
      }
    }
    if (is.null(getStoredExpressionChoices())){
      expression<- as.data.frame(names(V(g)))
      if(Dark_mode == F){
        expression$color <- rep(c("purple"))
      } else expression$color <- rep(c("#a419bc"))
      colnames(expression) <- c("id", "color")
    }

    #---------------------------------------------------#
    
    write(paste("
marker: {
    cmax: 2, 
    cmin: 1, 
    line: {
      color: 'rgb(50,50,50)', 
      width: 0.5
    }, 
    size: 3, 
    symbol: 'dot', 
    " , sep=""), file = fileConn, append = T) 
    
    if(Dark_mode==F){
      node_colors <- "purple"
    }else{
      node_colors <- "#a419bc"
    }
    
    if(expression_colors_3D == F){
      write(paste("color:'", node_colors, "'" , sep=""), file = fileConn, append = T)
    }else{
      write(paste("color:[", sep = ""), file = fileConn, append = T)
      
      for(i in 1:nrow(expression)){
      write(paste("'", expression$color[i], "'," , sep = ""), file = fileConn, append = T)
      }
      write(paste("]" , sep = ""), file = fileConn, append = T)
      
    }
    
    #// color: [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2], 
    #// colorscale: 'Viridis'
   
  write(paste("
  },
              text: [" , sep=""), file = fileConn, append = T)
  
  annotations_split <- str_split(annotation_graph[,2], ",", simplify = T)
  if(show_some_labels_3D == T){
    selected <- matrix("", ncol=1, nrow=0)
    for(i in 1:length(s)){
      selected <- rbind(selected, as.matrix(annotations_split[s[i],]))
    }
    selected <- unique(selected)
    selected <- selected[!is.na(selected)]
    selected <- selected[!selected==""]
    
    for(i in 1:length(node_names_3D)){
      if (node_names_3D[i] %in% selected){
        write(paste("\"",  node_names_3D[i], "\",", sep=""), file = fileConn, append = T, sep="")
      } else{
        write(paste("\"\",", sep=""), file = fileConn, append = T, sep="")
      }
    }
  }else{
  for(i in 1:length(node_names_3D)){
    if(i == length(node_names_3D)){
      cat(sprintf(paste("\"", node_names_3D[i], "\"", sep="")), file = fileConn)
    }else{
    cat(sprintf(paste("\"", node_names_3D[i], "\"", ",", sep="")), file = fileConn)
    }
  }
  }
    
  cat(sprintf(paste("],\n 
 hoverinfo: 'text'
                    };\n", sep="")), file = fileConn)
  

  traces<- c()
  for(i in 1:length(s)){
    if(i == length(s)){
      trace_i <- paste("trace", s[i], sep = "")
    } else{
      trace_i <- paste("trace", s[i], ",", sep = "")
    }
    
    xxxx <- c()
    yyyy <- c()
    zzzz <- c()
    for(j in 1:ncol(annotations_split)){
      if(annotations_split[s[i],j] != ""){
      xxxx <- c(xxxx, coordinates_hashmap[[ annotations_split[s[i],j]  ]][1])
      yyyy <- c(yyyy, coordinates_hashmap[[ annotations_split[s[i],j]  ]][2])
      zzzz <- c(zzzz, coordinates_hashmap[[ annotations_split[s[i],j]  ]][3])
      }
    }
    if(length(xxxx) < 5){
      xxxx <- paste(xxxx,  runif(10, xxxx[1] - 0.05, xxxx[1] + 0.05), sep= ",")
      yyyy <- paste(yyyy,  runif(10, yyyy[1] - 0.05, yyyy[1] + 0.05), sep= ",")
      zzzz <- paste(zzzz,  runif(10, zzzz[1] - 0.05, zzzz[1] + 0.05), sep= ",")
    }
    
    
    xxxx<- paste(xxxx, collapse=",")
    yyyy<- paste(yyyy, collapse=",")
    zzzz<- paste(zzzz, collapse=",")

    write(paste("trace",s[i],"= {\n name:'", annotation_graph[s[i],1], "',\n", 
                        "type: 'mesh3d', 
                      x: [", xxxx, "],
                      y: [", yyyy, "],
                      z: [", zzzz, "],
  color:'", qual_col_pals[s[i]], 
                        "',\n opacity: 0.2, 
  alphahull: 1, // 1 ορ 0.5
  showscale: true,
  text: [],
 
 hoverinfo: 'text'
};\n" , sep=""), file = fileConn, append = T)
    
    
    traces<- c(traces, trace_i) 
  }
  
  traces<- paste(traces, collapse=" ")
  
  if(Dark_mode == T){
    cat(sprintf(paste("var layout = {
	paper_bgcolor: 'black',
    plot_bgcolor: '#c7c7c7',", sep="")), file = fileConn)
  }else{
  cat(sprintf(paste("var layout = {", sep="")), file = fileConn)
  }
    
  if(show_labels_3D == T){
    hovermode <- "hovermode: false,"
  }else{
    hovermode <- "hovermode: true,"
  }
    cat(sprintf(paste(hovermode,
    "scene: {
      xaxis: {
      showspikes: false,
      	  range: [",scene_scale_x_min, ",",scene_scale_x_max,"],
          title: '',
          autorange: false,
          showgrid: false,
          zeroline: false,
          showline: false,
          autotick: true,
          ticks: '',
          showticklabels: false
      },
      yaxis: {
      showspikes: false,
          range: [",scene_scale_y_min, ",",scene_scale_y_max,"],
          title: '',
          autorange: false,
          showgrid: false,
          zeroline: false,
          showline: false,
          autotick: true,
          ticks: '',
          showticklabels: false
      },
      zaxis: {
      showspikes: false,
          range: [",scene_scale_z_min,",", scene_scale_z_max,"],
          title: '',
          autorange: false,
          showgrid: false,
          zeroline: false,
          showline: false,
          autotick: true,
          ticks: '',
          showticklabels: false
      }
    },
	 width: 1100,
  height: 900,
    margin: {
        l: 0,
        r: 0,
        b: 0,
        t: 0
    },
    showlegend: false,
    legend: {
        \"x\": \"0\",
        \"margin.r\": \"120\"
    }
};", sep="")), file = fileConn)

  write(paste("
  data = [",traces_edges,",trace_nodes,", traces,"]; 

Plotly.plot('plotly-div', {
  data: data,
  layout: layout
});
     </script>
   </body>
 </html> 
                    ", sep = ""), file = fileConn, append = T)
  
  
  
  } # if(length(s))
}