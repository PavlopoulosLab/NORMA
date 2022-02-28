# main functions ####
convex_hulls <- function(){
  set.seed(123)
  
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g))  return()
  annotation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  if (is.null(annotation_graph)) return()
  original_dataset_weighted <- fetchFirstSelectedStoredDataset_annotations_tab()
  if (is.null(original_dataset_weighted)) return(NULL)
  
  
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(Source = my_network$V1, Target = my_network$V2)

  annotation_graph <- as.data.frame(annotation_graph)
  groups<-annotation_graph
  
  annotation1 <- groups
  
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
  
  dataset1<- get.edgelist(g)
  dataset1 <- as.matrix(dataset1)
  annotation1 <- as.matrix(annotation1)
  
  nrowdat <- nrow(dataset1)
  nrowannot <- nrow(annotation1)
  
  if(length(nodes_with_NA_groups)>0){
    for (i in 1:length(nodes_with_NA_groups)){
      members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
    }
    members_with_NA_groups<-unique(members_with_NA_groups)
  }
  
  #---------------------------------#
  if (input$convex_layout_strategy == 'Supernodes per group'){
    shinyjs::show("repeling_force")
    shinyjs::show("local_layout")
  } else if (input$convex_layout_strategy == 'Group gravity'){
    shinyjs::show("repeling_force")
    shinyjs::hide("local_layout")
  } else {
    shinyjs::hide("repeling_force")
    shinyjs::hide("local_layout")
  }
  
  if(input$convex_layout_strategy == 'Simple layout'){
    lay <- layout_choices(g, input$layouts)
    node_name <- names(V(g))
  } else if(input$convex_layout_strategy == 'Virtual node per group'){
    lay <- convexStrategy1(g, annotation_graph)
    node_name <- unique(members_with_NA_groups$id)
  } else if(input$convex_layout_strategy == 'Group gravity'){
    result <- convexStrategy2(g, annotation_graph, input$layouts, input$repeling_force)
    lay <- result$lay
    node_name <- result$network_nodes
  } else if(input$convex_layout_strategy == 'Supernodes per group'){
    result <- convexStrategy3(g, annotation_graph, input$layouts, input$local_layout, input$repeling_force)
    lay <- result$lay
    node_name <- result$network_nodes
  }
  node_name_links <- node_name
  
  # Opening out conn
  fileConn <- file(paste("output_convex_",Sys.getpid(),".html", sep=""), "w")
  
  s <- input$chooseGroups_rows_selected
  if (length(s)==0)
  {
    s<-c(1:nrowannot)
  }
  
  if (length(s)) {
    s<-sort(s)
    x<- length(s)
    ccc<-group_pal_rows(length(x))
    tmp_selected_colors<- c()
    
    tmp_selected_colors<- c(tmp_selected_colors, ccc[s[i]])
    group_color <- tmp_selected_colors
    group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)
    
    cat(sprintf(paste("<!DOCTYPE html>
  <meta charset=\"utf-8\">
              
  <!-- Load d3.js -->
  <!--<script type=\"text/javascript\" src=\"d3.js\"></script>-->
  <script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/3.4.11/d3.min.js\"></script>
  
  <style>
  .node {
    stroke: #fff;
    stroke-width: 1.5px;
  }
  
  .nodelabel {
  font-family: \"arial\";
  font-size: ",scaling_labels_convex(), "px;
  }
  
  .link {
    stroke: #999;
    stroke-opacity: .6;
  }
  

  </style>
  	
  <body>
  	<script>
  	
  	
  	var width =",max_pixels_panel,",
  		height =", max_pixels_panel,";
            
          ",sep="")), file = fileConn)
    
    #   // The color functions: in this example I'm coloring all the convex hulls at the same layer the same to more easily see the result.
    # 		var color_value = [\"#63b598\", \"#ce7d78\", \"#ea9e70\", \"#a48a9e\", \"#c6e1e8\", \"#648177\", \"#0d5ac1\", 
    #                     \"#f205e6\", \"#1c0365\", \"#14a9ad\", \"#4ca2f9\", \"#a4e43f\", \"#d298e2\", \"#6119d0\",
    #                     \"#d2737d\", \"#c0a43c\", \"#f2510e\", \"#651be6\", \"#79806e\", \"#61da5e\", \"#cd2f00\", 
    #                     \"#9348af\", \"#01ac53\", \"#c5a4fb\", \"#996635\",\"#b11573\", \"#4bb473\", \"#75d89e\", 
    #                     \"#2f3f94\", \"#2f7b99\"];
    # 		
    #   
    
    
    for(i in 1:x){
      cat(sprintf(
        paste("groupHullColor", s[i], " = '", qual_col_pals[s[i]], "' ;\n", sep = "")), file = fileConn)
    }
    cat(sprintf("var force = d3.layout.force()
    .charge(-120)
    .linkDistance(30)
    .size([width, height]);

var zoomFlag = 0;

var zoomFlag = 0;
		var svg = d3.select(\"body\").append(\"svg\")
			.attr(\"width\", width)
			.attr(\"height\", height)
			.attr(\"transform\", \"translate(5.684341886080802e-14,5.684341886080802e-14) scale(0.9999999999999999)\") //TODO

			.call(d3.behavior.zoom().on(\"zoom\", function () {
			svg.attr(\"transform\", \"translate(\" + d3.event.translate + \")\" + \" scale(\" + d3.event.scale + \")\")
			//alert(d3.event.scale)
			if (!zoomFlag && d3.event.scale < 0.6){
				var nodelabels = g.selectAll(\".nodelabel\")
				nodelabels.text(\"\")
				zoomFlag = 1
			}
			if (zoomFlag && d3.event.scale > 0.6){
				var nodelabels = g.selectAll(\".nodelabel\")
				.text(function(d){return d.name;});
				zoomFlag = 0
			}
		  }))
		  
  .on(\"dblclick.zoom\", null)
  .on(\"mousedown.zoom\", null)
  .append(\"g\");
  
var g = d3.select(\"g\");

// propertyValue depicts the size of the node while value the edge width, 'fixed': true to disable bouncy physics
var theGraphData = {
\"nodes\":[\n"), file= fileConn)
    
    
    #--------------------------------#
    
    minx<-min(lay[,1])
    maxx<-max(lay[,1])
    miny<-min(lay[,2])
    maxy<-max(lay[,2])
    
    
    # Expressions
    if (!is.null(getStoredExpressionChoices())){
      expression<-fetchFirstSelectedStoredExpression()
      colnames(expression) <- c("id", "color")
      express_order<- as.data.frame(names(V(g)))
      colnames(express_order) <- "id"
      expression <- suppressMessages(left_join(express_order, expression, by = "id"))
      expression$color <- as.character(expression$color)
      for(i in 1:length(expression$color)){
        if(expression$color[i] == "" || is.na(expression$color[i])){
          expression$color[i] <- "lightgray"
        }
      }
      # expression$color <- gsub("-|\\s+|^$","lightgray", expression$color)
    }
    
    if (is.null(getStoredExpressionChoices())){
      expression<- as.data.frame(names(V(g)))
      expression$color <- rep(c("lightgray"))
      colnames(expression) <- c("id", "color")
    }
    
    zoom_slider<-TRUE
    max_allowed_scale<-1
    
    new_g<- get.edgelist(g)
    new_nodes<-unique(union(new_g[,1], new_g[,2]))
    
    df1<- data.frame(V1= column1)
    df2<- data.frame(V1= column2)
    
    unique_nodes_network<- suppressMessages(full_join(df1,df2))
    
    annotations2<- as.character(annotation_graph[,2])
    genes_tmp <- strsplit(annotations2, ",")
    
    unique_nodes_annotations<- as.data.frame(unique(unlist(genes_tmp)))
    colnames(unique_nodes_annotations)<- "V1"
    
    merged<- suppressMessages(full_join(unique_nodes_network, unique_nodes_annotations))
    
    words_to_be_removed<- suppressMessages(anti_join(merged,unique_nodes_network))
    
    if(length(words_to_be_removed$V1)>0){
      showModal(modalDialog(
        title = "Important message",
        paste("Please remove the nodes below from the selected annotation file as they were not found in the selected network file.", 
              as.character(words_to_be_removed),".", "To automatically remove them, please see Help Pages (Input File - Troubleshooting).", sep = "\t"),
        easyClose = T
      ))
    }
    if(length(words_to_be_removed$V1)==length(unique_nodes_annotations$V1)){
      showModal(modalDialog(
        title = "Important message",
        "Please check if the selected annotation file corresponds to the selected network.",
        easyClose = T
      ))
    }
    
    for (i in 1:length(new_nodes)){
      coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
      coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
      
      if(  (coor_x*scaling_coordinates_convex())>max_pixels_panel | (coor_y*scaling_coordinates_convex())>max_pixels_panel     )
      {
        zoom_slider<-FALSE
        break
      }
    }
    
    for(slider_values in 1:10){
      allowed<-TRUE
      for (i in 1:length(new_nodes)){
        coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
        coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
        if(  (coor_x*slider_values)>max_pixels_panel | (coor_y*slider_values)>max_pixels_panel     )
        {
          allowed<-FALSE
          break
        }
      }
      if(allowed==TRUE){
        max_allowed_scale<-slider_values 
      }
    }
    
    selected_genes<-c()
    for(i in 1:x){
      genes <- strsplit(annotation1[s[i], 2], ",")$Nodes
      selected_genes<-unique(c(selected_genes, genes))
    }
    
    if(zoom_slider==TRUE)
    {
      
      for (i in 1:length(new_nodes)){
        coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
        coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
        
        if(input$some_labels)
          if(x!=number_of_groups)
            if(!(node_name[i] %in% selected_genes))
              node_name[i]<-""
        
        if(!input$show_labels)
          node_name<-rep("", length(node_name))
        if(input$expressions){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20 , ", 'y':", coor_y*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20, ", 'fixed': true, \"color_value\":\"", expression$color[i], "\"},\n",sep="")), file = fileConn)
        }
        
        if(!input$expressions){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20 , ", 'y':", coor_y*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20, ", 'fixed': true, \"color_value\":", "\"lightgrey\"", "},\n",sep="")), file = fileConn)
        }
      }
    }#if zoom_slider
    else
    {
      for (i in 1:length(new_nodes)){
        coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
        coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
        
        if(!input$show_labels)
          node_name<-""
        
        if(input$expressions){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20 , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20, ", 'fixed': true, \"color_value\":\"", expression$color[i], "\"},\n",sep="")), file = fileConn)
        }
        
        if(!input$expressions){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20 , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20, ", 'fixed': true, \"color_value\":", "\"lightgrey\"", "},\n",sep="")), file = fileConn)
        }
      }
    }
    
    #------------------------------------#
    
    if(!(is.weighted(g))){
      original_dataset_weighted <- cbind(original_dataset_weighted[,1:2],"Weight"=rep(1, nrow(original_dataset_weighted)))
    }
    
    # links_df_weighted <- original_dataset_weighted[order(match(original_dataset_weighted[,1],dataset1[,1])),]
    
    cat(sprintf("],
  \"links\":[\n"), file= fileConn)
    
    for (i in 1:nrowdat){
      cat(sprintf(paste("{\"source\":", which(node_name_links %in% dataset1[i,1])-1, 
                        ",\"target\":", which(node_name_links %in% dataset1[i,2])-1, ",\"value\":", 
                        original_dataset_weighted[i,3],"},\n",sep="")), file = fileConn)
    }
    
    cat(sprintf(
      "]
}

graph = theGraphData

// The data for grouping nodes.
// The groups are not partitions, some nodes belong to more than one group.
var " 
    ), file = fileConn)
    
    for(i in 1:length(s)){
      
      genes <- strsplit(annotation1[s[i], 2], ",")$Nodes
      
      if(i<x){
        cat(sprintf(paste("group",s[i]," = [[", sep="")), file = fileConn)
        for(j in 1:length(genes)){
          if(j<length(genes)){
            cat(sprintf(paste(which(node_name_links %in% genes[j])-1, ",", sep="")), file = fileConn)}
          if(j==length(genes)){
            cat(sprintf(paste(which(node_name_links %in% genes[j])-1, sep="")), file = fileConn)}
        }
        cat(sprintf("]],\n"), file = fileConn)
        
      }
      if(i==x)
      {
        cat(sprintf(paste("group",s[i]," = [[", sep="")), file = fileConn)
        for(j in 1:length(genes)){
          if(j<length(genes)){
            cat(sprintf(paste(which(node_name_links %in% genes[j])-1, ",", sep="")), file = fileConn)}
          if(j==length(genes)){
            cat(sprintf(paste(which(node_name_links %in% genes[j])-1, sep="")), file = fileConn)}
          
        }
        cat(sprintf("]];\n"), file = fileConn)      
      }
    }      
    
    for (i in 1:x){
      cat(sprintf(paste("var groupNodes", s[i], " = group", s[i], 
                        ".map(function(group", s[i], ",index){
  return group", s[i],".map(function(member){return graph.nodes[member] });
  });\n", sep="")), file = fileConn)
    }
    
    cat(sprintf("\nvar groupPath = function(d) {
    var fakePoints = [];  
    if (d.length == 1 || d.length == 2) {     // This adjusts convex hulls for groups with fewer than 3 nodes by adding virtual nodes.
       fakePoints = [ [d[0].x + 0.001, d[0].y - 0.001],[d[0].x - 0.001, d[0].y + 0.001],[d[0].x - 0.001, d[0].y + 0.001]]; }     
    d.forEach(function(element) { fakePoints = fakePoints.concat([   // \"0.7071\" is the sine and cosine of 45 degree for corner points.
           [(element.x), (element.y + (2 + (4 * element.propertyValue)))],
           [(element.x + 0.7071 * (2 + (4 * element.propertyValue))), (element.y + 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x + (2 + (4 * element.propertyValue))), (element.y)],
           [(element.x + 0.7071 * (2 + (4 * element.propertyValue))), (element.y - 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x), (element.y - (2 + (4 * element.propertyValue)))],
           [(element.x - 0.7071 * (2 + (4 * element.propertyValue))), (element.y - 0.7071 * (2 + (4 * element.propertyValue)))],
           [(element.x - (2 + (4 * element.propertyValue))), (element.y)],
           [(element.x - 0.7071 * (2 + (4 * element.propertyValue))), (element.y + 0.7071 * (2 + (4 * element.propertyValue)))]
    ]); })
    return \"M\" + d3.geom.hull( fakePoints ).join(\"L\") + \"Z\";
};\n"), file = fileConn)
    
    for (i in 1:x){
      cat(sprintf(paste("\nvar groupHullFill", s[i]," = function(d, i) { return groupHullColor", s[i],"; };",sep="")
      ), file = fileConn)
    }
    
    cat(sprintf(paste("\nforce
.nodes(graph.nodes)
.links(graph.links)
.start();

var link = g.selectAll(\".link\")
    .data(graph.links)
	.enter().append(\"line\")
    .attr(\"class\", \"link\")
    .style(\"stroke-width\", function(d) { return Math.sqrt(d.value); });

var node = g.selectAll(\".node\")
    .data(graph.nodes)
	.enter().append(\"circle\")
    .attr(\"class\", \"node\")
    .attr(\"r\", function(d) { return 2 + (4 * d.propertyValue); })
	  .style(\"fill\", function(d) { return (d.color_value); })
    .style(\"stroke-width\", 1.5)
    .call(force.drag);
  
//node.append(\"title\")
//  .text(function(d) { return d.name; });
    
var nodelabels = svg.selectAll(\".nodelabel\") 
  .data(graph.nodes)
  .enter()
  .append(\"text\")
  .attr({\"x\":function(d){return d.x;},
        \"y\":function(d){return d.y;},
        \"class\":\"nodelabel\",
        \"stroke\":\"black\"})
  .text(function(d){return d.name;});

force.on(\"tick\", function() {
    // this updates the links, but they are UNDER the convex hulls because the hulls are recreated every tick
    link.attr(\"x1\", function(d) { return d.source.x; })
        .attr(\"y1\", function(d) { return d.source.y; })
        .attr(\"x2\", function(d) { return d.target.x; })
        .attr(\"y2\", function(d) { return d.target.y; });

    node.attr(\"cx\", function(d) { return d.x; })
        .attr(\"cy\", function(d) { return d.y; });
        
    nodelabels.attr(\"x\", function(d) { return d.x; }) 
        .attr(\"y\", function(d) { return d.y+3; }); 
                  
    // this updates the convex hulls
    g.selectAll(\"path\").remove()
    
                  ", sep="")), file = fileConn)
    for (i in 1:x){
      cat(sprintf(paste("\n
  g.selectAll(\"path#group", s[i],"\")
  .data(groupNodes", s[i], ")
  .attr(\"d\", groupPath)
  .enter().insert(\"path\", \"circle\")
  .style(\"fill\", groupHullFill",s[i], ")
  .style(\"stroke\", groupHullFill", s[i], ")
  .style(\"stroke-width\", 35)
  .style(\"stroke-linejoin\", \"round\")
  .style(\"opacity\", .2)
  .attr(\"ID\",\"group\")
  .attr(\"d\", groupPath);
", sep="")), file = fileConn)}
    
    cat(sprintf("\n });
 

	</script>
</body>\n"), file = fileConn)
    
    
    close(fileConn)
    
    
  }#if (length(s)) 
}#function

# Function for Layouts with virtual nodes (per group) that pull all in-group nodes
# @param g(igraph obj): the selected network
# @param groups(dataframe): the selected annotation file -> names and respective nodes
# @return lay: the layout coordinates
convexStrategy1 <- function(g, groups){
  if (is.null(g) || is.null(groups)) return()
  set.seed(123)
  
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(Source = my_network$V1, Target = my_network$V2)
  
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
  
  if(length(nodes_with_NA_groups)>0){
    for (i in 1:length(nodes_with_NA_groups))
    {
      members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
    }
    members_with_NA_groups<-unique(members_with_NA_groups)
  }
  
  edge <- data_frame(Source = my_network$Source, Target = my_network$Target, group = NA) #edge --> not edges
  
  within_group_edges <- members %>%
    split(.$group) %>%
    map_dfr(function (grp) {
      if(length(grp$id)>=2){
        id2id <- combn(grp$id, 2)
        data_frame(Source = id2id[1,],
                   Target = id2id[2,],
                   group = unique(grp$group))
      }
    })
  
  # sort by group as in file
  group_order<-(as.list(unique(members_with_NA_groups$group)))
  EE <- new.env(hash = TRUE)
  EE_positions <- new.env(hash = TRUE)
  for(i in 1: length(group_order))
  {
    group_name_as_key<-group_order[[i]]
    EE[[ as.character(group_name_as_key) ]]<-i
    EE_positions[[ as.character(i) ]]<-group_order[[i]]
  }
  for(i in 1: length(group_order))
  {
    group_name_as_key<-group_order[[i]]
    index<-EE[[ as.character(group_name_as_key) ]]
  }
  
  group_ids_tmp <- lapply(members_with_NA_groups %>% split(.$group), function(grp) { grp$id })
  group_ids<-c()
  for(i in 1: length(group_ids_tmp))
  {
    group_ids<-c(group_ids, group_ids_tmp[ EE_positions[[ as.character(i) ]]])
  }
  
  virt_group_nodes <- length(members_with_NA_groups$id) + 1:number_of_groups
  names(virt_group_nodes) <- unique(members$group) # altered from c(letters[1:number_of_groups])
  edges_virt <- data_frame(Source = edge$Source, Target = edge$Target, weight = 5, group = edge$group)
  
  within_virt <- members %>% split(.$group) %>% map_dfr(function (grp) {
    group_name <- unique(grp$group)
    virt_from <- rep(virt_group_nodes[group_name], length(grp$id))
    if(length(grp$id)>=2){
      id2id <- combn(grp$id, 2)
      data_frame(
        Source = c(id2id[1,], virt_from),
        Target = c(id2id[2,], grp$id),            # also connects from virtual_from node to each group node
        weight = c(rep(0.1, ncol(id2id)),     # weight between group nodes
                   rep(50, length(grp$id))),
        to_be_deleted = c(rep(T, ncol(id2id)),     # weight between group nodes
                          rep(T, length(grp$id))), # weight that 'ties together' the group (via the virtual group node)
        group = group_name
      )
    }
  })
  
  edges_virt <-bind_rows(mutate_all(edges_virt, as.character), mutate_all(within_virt, as.character)) # vgazei 38,39,40
  virt_group_na <- virt_group_nodes[is.na(names(virt_group_nodes))]
  non_group_nodes <- (members_with_NA_groups %>% filter(is.na(group)))$id
  nodes_virt <- data_frame(id = 1:(length(members_with_NA_groups$id) + length(virt_group_nodes)),
                           is_virt = c(rep(FALSE, length(members_with_NA_groups$id)),
                                       rep(TRUE, length(virt_group_nodes))))
  nodes_virt$id <- as.character(nodes_virt$id)
  
  #replace with the right names from our network
  nodes_virt[1:length(members_with_NA_groups$id), ]$id <- members_with_NA_groups$id 
  
  nodes_virt <- unique(nodes_virt)
  
  edge_names <- unique(c(edges_virt$Source, edges_virt$Target))
  nodes_virt <- nodes_virt[which(nodes_virt$id %in% edge_names), ]
  g_virt <- graph_from_data_frame(edges_virt, directed = FALSE, vertices = nodes_virt)
  
  # use "auto layout"
  # lay2 <- layout_nicely(g_virt)
  lay <- layout_choices(g_virt, input$layouts)
  
  # remove virtual group nodes from graph
  nodes_to_remove <- nodes_virt[which(nodes_virt$is_virt),]$id
  nodes_to_remove <- nodes_to_remove[nodes_to_remove %in% V(g_virt)$name]
  if (!identical(nodes_to_remove, character(0)))
    g_virt <- delete_vertices(g_virt, nodes_to_remove)
  
  # remove virtual group nodes' positions from the layout matrix
  tmp<-which(nodes_virt$is_virt == T )
  
  lay <- lay[-tmp, ]
  return(lay)
}

# Function for Layouts with enhanced gravity for in-group nodes
# @param g(igraph obj): the selected network
# @param groups(dataframe): the selected annotation file -> names and respective nodes
# @param layout(string): the user-selected layout choice
# @param repeling_force(int): the user-selected repeling force
# @return lay (2d double matrix): the layout coordinates
# @return network_nodes (character vector): the proper order of nodes(names) to correctly attach to canvas
convexStrategy2 <- function(g, groups, layout, repeling_force){
  lay <- NULL
  network_nodes <- NULL
  if (!(is.null(g) || is.null(groups))){
    set.seed(123)
    
    # network
    my_network <- as.data.frame(get.edgelist(g))
    my_network <- cbind(my_network, as.double(E(g)$Weight))
    colnames(my_network) <- c('Source', 'Target', 'Weight')
    network_nodes <- unique(c(my_network$Source, my_network$Target))
    
    # annotations
    groups_expanded <- groups %>% separate_rows(Nodes, sep=",")
    groups_expanded <- groups_expanded[which(groups_expanded$Nodes %in% network_nodes), ] # removing non-existing nodes
    
    # 1. create dataframe with extra edges (all against all in same groups that do not already exist in my_network)
    extra_edges <- merge(groups_expanded, groups_expanded, by.x = "Annotations", by.y = "Annotations")
    temp_g <- graph_from_data_frame(extra_edges[, c(2,3)], directed = F)
    if ('Kamada-Kawai' == str_split(layout, "\t")[[1]][1]) E(temp_g)$weight <- min(my_network$Weight)
    else E(temp_g)$weight <- max(my_network$Weight) # * 1.0001 # invisible weight = max network value *2
    temp_g <- igraph::simplify(temp_g, remove.multiple = T, remove.loops = T, edge.attr.comb = "first")
    extra_edges <- as.data.frame(cbind( get.edgelist(temp_g) , E(temp_g)$weight ))
    colnames(extra_edges) <- c('Source', 'Target', 'Weight')
    
    # 2. check network edges one by one; if exist in same group, weight * 100, else weight/100
    # This brings nodes ultra-close - unneeded
    for (i in 1:nrow(my_network)){
      source_groups <- groups_expanded[which( groups_expanded$Nodes %in%  my_network$Source[i]), ]$Annotations
      target_groups <- groups_expanded[which( groups_expanded$Nodes %in%  my_network$Target[i]), ]$Annotations
      if ('Kamada-Kawai' == str_split(layout, "\t")[[1]][1]){
        if (identical(source_groups, character(0)) ||
            identical(target_groups, character(0)) ||
            identical(intersect(source_groups, target_groups), character(0)))
          my_network$Weight[i] <- my_network$Weight[i] * repeling_force # kamada-kawai swap
        else my_network$Weight[i] <- my_network$Weight[i] / repeling_force
      } else {
        if (identical(source_groups, character(0)) ||
            identical(target_groups, character(0)) ||
            identical(intersect(source_groups, target_groups), character(0)))
          my_network$Weight[i] <- my_network$Weight[i] / repeling_force # kamada-kawai swap
        else my_network$Weight[i] <- my_network$Weight[i] * repeling_force
      }
    }
    
    # 3. append to my_network
    my_network <- rbind(my_network, extra_edges)
    
    # 4. graph handling
    out_g <- graph_from_data_frame(my_network, directed = F, vertices = network_nodes)
    E(out_g)$weight <- as.numeric(my_network$Weight)
    out_g <- igraph::simplify(out_g, remove.multiple = T, remove.loops = T, edge.attr.comb = "max")
    lay <- layout_choices(out_g, layout)
  }
  
  return(list(lay = lay, network_nodes = network_nodes, groups_expanded = groups_expanded))
}

# Function for Layouts with superNodes per Annotation Group
# @param g(igraph obj): the selected network
# @param groups(dataframe): the selected annotation file -> names and respective nodes
# @param layout(string): the user-selected layout choice
# @param local_layout(string): the user-selected layout choice for in-group layouts
# @param repeling_force(int): the user-selected repeling force
# @return lay (2d double matrix): the layout coordinates
# @return network_nodes (character vector): the proper order of nodes(names) to correctly attach to canvas
convexStrategy3 <- function(g, groups, layout, local_layout, repeling_force){
  lay <- NULL
  network_nodes <- NULL
  if (!(is.null(g) || is.null(groups))){
    set.seed(123)

    # network
    my_network <- as.data.frame(get.edgelist(g))
    my_network <- cbind(my_network, as.double(E(g)$Weight))
    colnames(my_network) <- c('Source', 'Target', 'Weight')
    network_nodes <- unique(c(my_network$Source, my_network$Target))
    
    # annotations
    groups_expanded <- groups %>% separate_rows(Nodes, sep=",")
    groups_expanded <- groups_expanded[which(groups_expanded$Nodes %in% network_nodes), ] # removing non-existing nodes
    
    noGroupNodes <- network_nodes[!(network_nodes %in% groups_expanded$Nodes)]
    
    # 1. create dataframe of one supernode per group plus no-group nodes
    # Source Target -> swap all nodes with their respective Group Name(s)
    # if multiple groups per node, add the extra edges
    # e.g. Group1+2Node - noGroupNode -> Group1 - noGroupNode, Group2 - noGroupNode
    # merge my_network with groups_expanded two times ( Source - Nodes, Target - Nodes)
    # where annotations not NA, swap Source or Target with respective Group Name
    superFrame <- merge(my_network, groups_expanded, by.x = 'Source', by.y = 'Nodes', all.x = T)
    superFrame <- merge(superFrame, groups_expanded, by.x = 'Target', by.y = 'Nodes', all.x = T)
    graphFrame <- superFrame # keeping this for later on
    graphFrame$Source[!is.na(graphFrame$Annotations.x)] <- graphFrame$Annotations.x[!is.na(graphFrame$Annotations.x)]
    graphFrame$Target[!is.na(graphFrame$Annotations.y)] <- graphFrame$Annotations.y[!is.na(graphFrame$Annotations.y)]
    graphFrame <- graphFrame[, c('Source', 'Target', 'Weight')]
    
    # 2. create graph and apply layout on this compound supernode network
    temp_g <- graph_from_data_frame(graphFrame, directed = F)
    E(temp_g)$weight <- as.numeric(graphFrame$Weight)
    temp_g <- igraph::simplify(temp_g, remove.multiple = T, remove.loops = T, edge.attr.comb = "max")
    lay_super <- layout_choices(temp_g, layout)
    # lay_super <- layout_with_fr(temp_g)
    lay_super <- cbind(lay_super, names(V(temp_g)))
    # plot(temp_g, layout = lay_super)
    
    # 3. push all nodes above away from 0,0 // also check minx maxx for layout as alternative
    # foreach node, calculate a = y/x
    # then multiply x by an input number n (e.g.) and solve for y
    # keep the (x, y) coords system in a matrix for all supernodes
    lay_super <- as.data.frame(lay_super)
    lay_super$V1 <- as.numeric(lay_super$V1)
    lay_super$V2 <- as.numeric(lay_super$V2)
    lay_super$a <- ifelse(lay_super$V1 != 0, lay_super$V2 / lay_super$V1, lay_super$V2 / 0.01)
    lay_super$X <- repeling_force * lay_super$V1
    lay_super$Y <- lay_super$a * lay_super$X
    lay_super <- lay_super[, c('X', 'Y', 'V3')]
    colnames(lay_super)[3] <- 'Node'
    
    # 4. foreach group, add low-weight within group edges and
    # apply layout (2nd input choice) with the respective (x,y) coords system
    # extra edges dataframe for all groups
    extra_edges <- merge(groups_expanded, groups_expanded, by.x = "Annotations", by.y = "Annotations")
    lay <- matrix(, nrow = 0, ncol = 3)
    for (group in unique(groups_expanded$Annotations)){
      tempFrame <- superFrame
      tempFrame <- tempFrame[(tempFrame$Annotations.x == group & tempFrame$Annotations.y == group), ]
      tempFrame <- tempFrame[!is.na(tempFrame$Source) & !is.na(tempFrame$Target), ]
      if (nrow(tempFrame) > 0){
        tempFrame <- tempFrame[, c('Source', 'Target', 'Weight')]
        
        # create any missing in-group edges with minimum weight
        # (all against all in same groups that do not already exist in tempFrame)
        temp_extra_edges <- extra_edges[extra_edges$Annotations == group, ]
        temp_g <- graph_from_data_frame(temp_extra_edges[, c(2,3)], directed = F)
        if ('Kamada-Kawai' == str_split(layout, "\t")[[1]][1]) E(temp_g)$weight <- min(tempFrame$Weight) * repeling_force
        else E(temp_g)$weight <- min(tempFrame$Weight) / repeling_force # * 1.0001 # invisible weight = max network value *2
        temp_g <- igraph::simplify(temp_g, remove.multiple = T, remove.loops = T, edge.attr.comb = "first")
        temp_extra_edges <- as.data.frame(cbind( get.edgelist(temp_g) , E(temp_g)$weight ))
        colnames(temp_extra_edges) <- c('Source', 'Target', 'Weight')
        
        tempFrame <- rbind(tempFrame, temp_extra_edges)
        
        # TODO check 1 and 2 node layout conditions
        temp_g <- graph_from_data_frame(tempFrame, directed = F)
        E(temp_g)$weight <- as.numeric(tempFrame$Weight)
        temp_g <- igraph::simplify(temp_g, remove.multiple = T, remove.loops = T, edge.attr.comb = "max")
        temp_lay <- layout_choices(temp_g, local_layout)
        # temp_lay <- layout_with_fr(temp_g)
        temp_lay <- cbind(temp_lay, names(V(temp_g)))
        # plot(temp_g, layout = temp_lay)
        
        groupX <- lay_super[lay_super[,3] == group, 1]
        groupY <- lay_super[lay_super[,3] == group, 2]
        temp_lay[, 1] <- as.numeric(temp_lay[, 1]) + groupX
        temp_lay[, 2] <- as.numeric(temp_lay[, 2]) + groupY
        
        lay <- rbind(lay, temp_lay)
      } else{
        lay <- rbind(lay, c(lay_super[lay_super[,3] == group, 1],
                            lay_super[lay_super[,3] == group, 2],
                            groups$Nodes[groups$Annotations == group]))
      }
    } # end for

    # 5. calculate coordinates for duplicate nodes
    dflay <- as.data.frame(lay)
    dflay$V1 <- as.numeric(dflay$V1)
    dflay$V2 <- as.numeric(dflay$V2)
    meanX <- aggregate(dflay$V1, by=list(dflay$V3), FUN=mean)
    colnames(meanX) <- c("Node", "X")
    meanY <- aggregate(dflay$V2, by=list(dflay$V3), FUN=mean)
    colnames(meanY) <- c("Node", "Y")
    dflay <- merge(meanX, meanY)
    dflay <- as.matrix(dflay[, c("X", "Y", "Node")])
    
    # 6. append non-group nodes from lay_super
    lay_noGroupNodes <- lay_super[lay_super[,3] %in% noGroupNodes, ]
    lay <- rbind(dflay, lay_noGroupNodes)
    network_nodes <- lay[, 3]
    
    lay <- cbind(as.numeric(lay[, 1]), as.numeric(lay[, 2]))
  }
  
  return(list(lay = lay, network_nodes = network_nodes, groups_expanded = groups_expanded))
}

pie_charts<- function(){
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g)) 
    return()
  dataset1<- get.edgelist(g)
  
  original_dataset_weighted <- fetchFirstSelectedStoredDataset_annotations_tab()
  if (is.null(original_dataset_weighted))
    return(NULL)
  
  
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(Source = my_network$V1, Target = my_network$V2)
  
  annotation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  if (is.null(annotation_graph)) return()
  annotation_graph <- as.data.frame(annotation_graph)
  groups<-annotation_graph
  
  annotation1<- groups
  
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
  dataset1 <- as.matrix(dataset1)
  annotation1 <- as.matrix(annotation1)
  
  nrowdat <- nrow(dataset1)
  nrowannot <- nrow(annotation1)
  
  s <- input$chooseGroups2_rows_selected
  if (length(s)==0)
  {
    s<-c(1:nrowannot)
  }
  
  if (length(s)) {
    s<-sort(s)
    x<- length(s)
    ccc<-group_pal_rows(length(x))
    tmp_selected_colors<- c()
    
    tmp_selected_colors<- c(tmp_selected_colors, ccc[s[i]])
    group_color <- tmp_selected_colors
    group_color_fill <- adjustcolor(group_color, alpha.f = 0.2)
    
    fileConn <- file(paste("output_pies_",Sys.getpid(),".html", sep=""), "w")
    cat(sprintf(paste("<!DOCTYPE html>
<head>
  <meta charset=\"utf-8\">
  <script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/3.4.11/d3.min.js\"></script>
  <style>
    .node {
            stroke: white;
			stroke-width: 2;
        }

	.nodelabel {
	  font-family: \"arial\";
	   font-size:",scaling_labels_pies() ,"px;

	}

	.link {
	  stroke: #999;
	  stroke-opacity: .6;
	}
  </style>
</head>

<body>
  <script>
     
		var graph = { 	\"nodes\":[\n", sep="")), file = fileConn)
    
    if(length(nodes_with_NA_groups)>0){
      for (i in 1:length(nodes_with_NA_groups))
      {
        members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
      }
      members_with_NA_groups<-unique(members_with_NA_groups)
    }
    nodes <- unique(members_with_NA_groups$id)
    annots <- na.omit(members_with_NA_groups)
    annots<- unique(annots$group)
    
    members_with_zeros<- as.matrix(members_with_NA_groups)
    members_with_zeros[is.na(members_with_zeros)] <- 0
    members_with_zeros<- as.data.frame(members_with_zeros)
    
    groupss <- members_with_zeros %>% 
      group_by(id) %>% 
      summarise_all(funs(trimws(paste(., collapse = ','))))
    groupss <- suppressMessages(inner_join(members_with_zeros, groupss, by = "id"))
    groupss <- data.frame(groupss[,1],groupss[,3])
    groupss <- groupss[!duplicated(groupss[,1]), ]
    colnames(groupss)<- c("V1", "V2")
    groupss <- as.data.frame(groupss)
    
    #---------------------------------#
    if (input$piechart_layout_strategy == 'Supernodes per group'){
      shinyjs::show("repeling_force2")
      shinyjs::show("local_layout2")
    } else if (input$piechart_layout_strategy == 'Group gravity'){
      shinyjs::show("repeling_force2")
      shinyjs::hide("local_layout2")
    } else {
      shinyjs::hide("repeling_force2")
      shinyjs::hide("local_layout2")
    }
    
    lay <- input$layouts2
    if(input$piechart_layout_strategy == 'Simple layout'){
      lay <- layout_choices(g, lay)
      node_name<-names(V(g))
      not_virtual_nodes <- groupss[order(match(groupss[,1],node_name)),]
      groupss_as_charachter<- as.character(not_virtual_nodes$V2)
    } else if(input$piechart_layout_strategy == 'Virtual node per group'){
      lay <- convexStrategy1(g, annotation_graph)
      node_name <- unique(members_with_NA_groups$id)
      groupss_as_charachter<- as.character(groupss$V2)
    } else if(input$piechart_layout_strategy == 'Group gravity'){
      result <- convexStrategy2(g, annotation_graph, input$layouts2, input$repeling_force2)
      lay <- result$lay
      node_name <- result$network_nodes
      groups_expanded <- result$groups_expanded

      groupss_as_charachter <- merge(as.matrix(node_name), groups_expanded, by.x = "V1", by.y ='Nodes', all.x = T, sort = F)
      groupss_as_charachter[which(is.na(groupss_as_charachter$Annotations)), ]$Annotations <- 0
      groupss_as_charachter <- groupss_as_charachter %>%
        group_by(V1) %>%
        summarize(annot = str_c(Annotations, collapse = ","))
      groupss_as_charachter<- as.matrix(groupss_as_charachter)
      groupss_as_charachter <- merge(as.matrix(node_name), groupss_as_charachter, by.x = "V1", by.y ='V1', all.x = T, sort = F)
      groupss_as_charachter<- groupss_as_charachter$annot
    } else if(input$piechart_layout_strategy == 'Supernodes per group'){
      result <- convexStrategy3(g, annotation_graph, input$layouts2, input$local_layout2, input$repeling_force2)
      lay <- result$lay
      node_name <- result$network_nodes
      groups_expanded <- result$groups_expanded
      
      # saveRDS(node_name, "node_name.RDS")
      # saveRDS(groups_expanded, "groups_expanded.RDS")

      groupss_as_charachter <- merge(as.matrix(node_name), groups_expanded, by.x = "V1", by.y ='Nodes', all.x = T, sort = F)
      groupss_as_charachter[which(is.na(groupss_as_charachter$Annotations)), ]$Annotations <- 0
      groupss_as_charachter <- groupss_as_charachter %>%
        group_by(V1) %>%
        summarize(annot = str_c(Annotations, collapse = ","))
      groupss_as_charachter<- as.matrix(groupss_as_charachter)
      groupss_as_charachter <- merge(as.matrix(node_name), groupss_as_charachter, by.x = "V1", by.y ='V1', all.x = T, sort = F)
      groupss_as_charachter<- groupss_as_charachter$annot
    }
    # saveRDS(node_name, "node_name.RDS")
    # saveRDS(annotation_graph, "annotation_graph.RDS")
    node_name_links <- node_name
    Groupss <- strsplit(groupss_as_charachter, ",")
    
    
    #--------------------------------#
    
    
    # Expressions
    
    if (!is.null(getStoredExpressionChoices())){
      expressions_pies<-fetchFirstSelectedStoredExpression()
      colnames(expressions_pies) <- c("id", "color")
      express_order<- as.data.frame(names(V(g)))
      colnames(express_order) <- "id"
      expressions_pies <- suppressMessages(left_join(express_order, expressions_pies, by = "id"))
      expressions_pies$color<- as.character(expressions_pies$color)
      # expressions_pies$color <- gsub("-|\\s+|^$","lightgray", expressions_pies$color)
      
      for(i in 1:length(expressions_pies$color)){
        if(expressions_pies$color[i] == "" || is.na(expressions_pies$color[i])){
          expressions_pies$color[i] <- "lightgray"
        }
      }
      
    }
    
    if (is.null(getStoredExpressionChoices())){
      expressions_pies<- as.data.frame(names(V(g)))
      expressions_pies$color <- rep(c("lightgray"))
      colnames(expressions_pies) <- c("id", "color")
    }
    
    minx<-min(lay[,1])
    maxx<-max(lay[,1])
    miny<-min(lay[,2])
    maxy<-max(lay[,2])
    
    
    pie_to_be_colored<-c(rep(F,length(nodes)))
    
    for (i in 1:length(nodes)){
      pie_to_be_colored[i]<-any(is.element(Groupss[[i]], annotation1[s])) 
    }
    
    zoom_slider<-TRUE
    max_allowed_scale<-1
    
    new_g<- get.edgelist(g)
    new_nodes<-unique(union(new_g[,1], new_g[,2]))
    
    df1<- data.frame(V1= column1)
    df2<- data.frame(V1= column2)
    
    unique_nodes_network <- suppressMessages(full_join(df1, df2))
    
    annotations2<- as.character(annotation_graph[,2])
    genes_tmp <- strsplit(annotations2, ",")
    
    unique_nodes_annotations<- as.data.frame(unique(unlist(genes_tmp)))
    colnames(unique_nodes_annotations)<- "V1"
    
    merged <- suppressMessages(full_join(unique_nodes_network, unique_nodes_annotations))
    
    words_to_be_removed <- suppressMessages(anti_join(merged,unique_nodes_network))
    
    if(length(words_to_be_removed$V1)>0){
      showModal(modalDialog(
        title = "Important message",
        paste("Please remove the nodes below from the selected annotation file as they were not found in the selected network file.", 
              as.character(words_to_be_removed),".", "To automatically remove them, please see Help Pages (Input File - Troubleshooting).", sep = "\t"),
        easyClose = T
      ))
    }
    if(length(words_to_be_removed$V1)==length(unique_nodes_annotations$V1)){
      showModal(modalDialog(
        title = "Important message",
        "Please check if the selected annotation file corresponds to the selected network.",
        easyClose = T
      ))
    }
    
    
    
    for (i in 1:length(new_nodes)){
      coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
      coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
      if(  (coor_x*scaling_coordinates_pies())>max_pixels_panel | (coor_y*scaling_coordinates_pies())>max_pixels_panel     )
      {
        zoom_slider<-FALSE
        break
      }
    }
    
    for(slider_values in 1:10){
      allowed<-TRUE
      for (i in 1:length(new_nodes)){
        coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
        coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
        if(  (coor_x*slider_values)>max_pixels_panel | (coor_y*slider_values)>max_pixels_panel     )
        {
          allowed<-FALSE
          break
        }
      }
      if(allowed==TRUE){
        max_allowed_scale<-slider_values 
      }
    }
    
    
    selected_genes<-c()
    for(i in 1:x){
      genes <- strsplit(annotation1[s[i], 2], ",")$Nodes
      selected_genes<-unique(c(selected_genes, genes))
    }
    
    if(zoom_slider==TRUE)
    {
      for (i in 1:length(new_nodes)){
        coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
        coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
        # node_name<-nodes[i]
        
        if(input$some_labels_pies)
          if(x!=number_of_groups)
            if(!(node_name[i]  %in% selected_genes))
              node_name[i]<-""
        
        if(!input$show_labels_pies)
          node_name<-rep("", length(node_name))
        if(input$expressions_pies){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20  , ", 'y':", coor_y*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20 , ", 'fixed': true, \"color_value\":\"", expressions_pies$color[i], "\",\"proportions\": [\n",sep="")), file = fileConn)
        }
        if(!input$expressions_pies){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20  , ", 'y':", coor_y*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20 , ", 'fixed': true, \"color_value\":", "\"lightgrey\"", ",\"proportions\": [\n",sep="")), file = fileConn)
        }
        
        if(pie_to_be_colored[i]==F){
          cat(sprintf(paste("{\"group\": 0," , "\"value\":", scaling_nodes_pies(), "}]},\n")), file = fileConn)
        }
        if(pie_to_be_colored[i]==T){
          counter<-1
          max_length<-length(intersect(Groupss[[i]], annotation1[s]))
          
          
          for (j in 1:length(Groupss[[i]])) {
            if(is.element(Groupss[[i]][j], annotation1[s]) == T){
              if(counter<max_length){
                cat(sprintf(paste("{\"group\":", which(annotation1[s] %in% Groupss[[i]][j]), "," , "\"value\":", scaling_nodes_pies(), "},\n")), file = fileConn)
              }
              if(counter==max_length){
                cat(sprintf(paste("{\"group\":", which(annotation1[s] %in% Groupss[[i]][j]), "," , "\"value\":", scaling_nodes_pies(), "}]},\n")), file = fileConn)
              }
              counter<-counter+1
            }
          }
        }
      }#for
    }#if zoom_slider
    else
    {
      for (i in 1:length(new_nodes)){
        coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
        coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)
        # node_name<-node_name[i]
        if(!input$show_labels_pies)
          node_name<-rep("", length(node_name))
        if(input$expressions_pies){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20  , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20 , ", 'fixed': true, \"color_value\":\"", expressions_pies$color[i], "\",\"proportions\": [\n",sep="")), file = fileConn)
        }
        if(!input$expressions_pies){
          cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20  , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20 , ", 'fixed': true, \"color_value\":", "\"lightgrey\"", ",\"proportions\": [\n",sep="")), file = fileConn)
        }
        
        if(pie_to_be_colored[i]==F){
          cat(sprintf(paste("{\"group\": 0," , "\"value\":", scaling_nodes_pies(), "}]},\n")), file = fileConn)
        }
        if(pie_to_be_colored[i]==T){
          counter<-1
          max_length<-length(intersect(Groupss[[i]], annotation1[s]))
          for (j in 1:length(Groupss[[i]])) {
            if(is.element(Groupss[[i]][j], annotation1[s]) == T){
              if(counter<max_length){
                cat(sprintf(paste("{\"group\":", which(annotation1[s] %in% Groupss[[i]][j]), "," , "\"value\":", scaling_nodes_pies(), "},\n")), file = fileConn)
              }
              if(counter==max_length){
                cat(sprintf(paste("{\"group\":", which(annotation1[s] %in% Groupss[[i]][j]), "," , "\"value\":", scaling_nodes_pies(), "}]},\n")), file = fileConn)
              }
              counter<-counter+1
            }
          }
        }
      }#for
    }
    
    
    if(!(is.weighted(g))){
      original_dataset_weighted <- cbind(original_dataset_weighted[,1:2],"Weight"=rep(1, nrow(original_dataset_weighted)))
    }
    
    cat(sprintf("],\n
						\"links\":[\n"), file = fileConn)
    for (i in 1:nrowdat){
      cat(sprintf(paste("{\"source\":", which(node_name_links %in% dataset1[i,1])-1, ",\"target\":", which(node_name_links %in% dataset1[i,2])-1, ",\"value_links\":", original_dataset_weighted[i,3] ,"},\n",sep="")), file = fileConn
      )}
    cat(sprintf(
      "]
    };\n "), file = fileConn)
    
    
    cat(sprintf("var width = 10000,
			height = 10000,
			radius = 25,
			color = d3.scale.linear().domain([",sep=""), file = fileConn)
    
    
    # Colors
    for(i in 0:x){
      cat(sprintf(paste(i, "," ,sep="")), file = fileConn)}
    
    vector_zero<- groupss$V2==0
    length_vector_zero<- sum(vector_zero, na.rm = TRUE)
    
    cat(sprintf("]).range([\"#cccccc\","), file = fileConn)
    for(i in 1:x){
      cat(sprintf(paste( "\"", qual_col_pals[s[i]], "\"," ,sep="")), file = fileConn)
    }
    cat(sprintf("])\n"), file = fileConn)
    
    cat(sprintf("
  var pie = d3.layout.pie()
			.sort(null)
			.value(function(d) { return d.value; });

		var arc = d3.svg.arc()
			.outerRadius(function(d) { return d.value; })
			.innerRadius(0);
		
		var zoomFlag = 0;
		
		var svg = d3.select(\"body\").append(\"svg\")
			.attr(\"width\", width)
			.attr(\"height\", height)
			//.style(\"border\", \"4px solid black\")
			.attr(\"transform\", \"translate(5.684341886080802e-14,5.684341886080802e-14) scale(0.9999999999999999)\") //TODO
			.call(d3.behavior.zoom().on(\"zoom\", function () {
			svg.attr(\"transform\", \"translate(\" + d3.event.translate + \")\" + \" scale(\" + d3.event.scale + \")\")
			//alert(d3.event.scale)
			if (!zoomFlag && d3.event.scale < 0.6){
				var nodelabels = g.selectAll(\".nodelabel\")
				nodelabels.text(\"\")
				zoomFlag = 1
			}
			if (zoomFlag && d3.event.scale > 0.6){
				var nodelabels = g.selectAll(\".nodelabel\")
				.text(function(d){return d.name;});
				zoomFlag = 0
			}
		  }))
		  .on(\"dblclick.zoom\", null)
		  .on(\"mousedown.zoom\", null)
		  .append(\"g\")
			.attr(\"transform\", \"translate(5.684341886080802e-14,5.684341886080802e-14) scale(0.9999999999999999)\");
		  
		var g = d3.select(\"g\");

		var force = d3.layout.force()
			.charge(-120)
			.linkDistance(100)
			.size([width, height]);

		force.nodes(graph.nodes)
			 .links(graph.links)
			 .start();

		var link = svg.selectAll(\".link\")
			.data(graph.links)
			.enter().append(\"line\")
			.attr(\"class\", \"link\")
			.style(\"stroke-width\", function(d) { return Math.sqrt(d.value_links); });

		var node = svg.selectAll(\".node\")
			.data(graph.nodes)
			.enter().append(\"g\")
			.attr(\"class\", \"node\")
			.style(\"stroke\", function(d) { return (d.color_value); })
			.call(force.drag);
			
		var nodelabels = svg.selectAll(\".nodelabel\") 
			.data(graph.nodes)
			.enter()
			.append(\"text\")
			.attr({\"x\":function(d){return d.x;},
				  \"y\":function(d){return d.y;},
				  \"class\":\"nodelabel\",
				  \"stroke\":\"black\"})
			.text(function(d){return d.name;});
			   
		node.selectAll(\"path\")
			.data(function(d, i) {return pie(d.proportions); })
			.enter()
			.append(\"svg:path\")
			.attr(\"d\", arc)
			.attr(\"fill\", function(d, i) { return color(d.data.group); });;

		force.on(\"tick\", function() {
			link.attr(\"x1\", function(d) { return d.source.x; })
			.attr(\"y1\", function(d) { return d.source.y; })
			.attr(\"x2\", function(d) { return d.target.x; })
			.attr(\"y2\", function(d) { return d.target.y; });

			node.attr(\"x\", function(d) { return d.x; })
				.attr(\"y\", function(d) { return d.y; })
				.attr(\"transform\", function(d) { return \"translate(\" + d.x + \",\" + d.y + \")\"});
				
			nodelabels.attr(\"x\", function(d) { return d.x; }) 
				.attr(\"y\", function(d) { return d.y+3; });
		});
  </script>
</body>
"), file = fileConn)
    
    close(fileConn)
  }#if (length(s)) 
}#function 

vennDiagrams <- function(){
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g)) return()
  
  my_network <- as.data.frame(get.edgelist(g))
  my_network <- data.frame(Source = my_network$V1, Target = my_network$V2)

  annotation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  
  if (is.null(annotation_graph))
    return()
  annotation_graph <- as.data.frame(annotation_graph)
  groups <- annotation_graph
  groups <- data.frame(V1 = groups$Annotations, stri_split_fixed(groups$Nodes, ",",  simplify = TRUE))
  groups <- mutate_all(groups, funs(na_if(., "")))
  number_of_groups <- dim(groups)[1]
  
  x <- list()
  for (i in 1:number_of_groups) {
    group_i <- groups[i, ]
    group_i <- group_i[, -1]
    group_i <- group_i[!is.na(group_i)]
    x[[i]] <- (group_i)
  }
  
  GO <- list()
  for (i in 1:number_of_groups) {
    GO[[i]] <- rep(groups[i, 1], length(x[[i]]))
  }
  
  column1 <- my_network$Source
  column2 <- my_network$Target
  node_names <- unique(union(column1, column2))
  tt <- unlist(x)
  nodes_with_NA_groups <- setdiff(node_names, tt)
  
  members <- data_frame(id = unlist(x), group = unlist(GO))
  members_with_NA_groups <-
    data_frame(id = unlist(x), group = unlist(GO))
  if (length(nodes_with_NA_groups) > 0) {
    for (i in 1:length(nodes_with_NA_groups))
    {
      members_with_NA_groups[nrow(members_with_NA_groups) + 1, 1] <-
        nodes_with_NA_groups[i]
    }
    members_with_NA_groups <- unique(members_with_NA_groups)
  }
  
  venn<- members_with_NA_groups %>% group_by(id) %>% summarise_all(funs(trimws(paste(., collapse = ','))))
  
  return(venn)
}

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

fetchDataset_annotations_tab <- function(nid) {
  retVal <- NULL
  if (length(nid) > 0) {
    retVal <- readRDS(paste0(nid, ".rda"))
    attr(retVal, "id") <- nid
  }
  return(retVal)
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
fetchFirstSelectedStoredIgraph_annotations_tab <- function() {
  dataset <- fetchFirstSelectedStoredDataset_annotations_tab()
  if (is.null(dataset))
    return(NULL)
  else
    return(convert_to_igraph(dataset))
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

# Output after choosing selected annotations
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

layout_choices_3D <- function(igraph, layouts_3D){
  if(length(layouts_3D)==0)
    return(NULL)
  results<-list()
  for(i in layouts_3D){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]]<-eval(parse(text=command))
  }
  
  return(results[[description]])
}

# functions for selectbox choices
layout_choices <- function(igraph,layouts_ui){
  if(length(layouts_ui)==0)
    return(NULL)
  results<-list()
  for(i in layouts_ui){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]]<-eval(parse(text=command))
  }
  return(results[[description]])
}
