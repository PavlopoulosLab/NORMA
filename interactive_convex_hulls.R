convex_hulls<- function(){
  
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g)) 
    return()
  dataset1<- get.edgelist(g)
  
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(Source = my_network$V1, Target = my_network$V2)
  gName <- SelectedStoredNets()$name
  
  annoation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  if (is.null(annoation_graph)) 
    return()
  
  annotName <- SelectedStoredAnnots()$name
  
  annoation_graph <- as.data.frame(annoation_graph)
  groups<-annoation_graph
 
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

  
 if(layouts_with_virtual_nodes==T){
  source("convex_hulls_layout_virtual_nodes.R", local = T)
  lay<-convexInput()}
  else{
    set.seed(123)
    lay <- layout_choices(g, lay)
  }

  fileConn <- file(paste("output_convex_",Sys.getpid(),".html", sep=""), "w")
  
  if (length(s)==0)
  {
    s<-c(1:nrowannot)
  }
  
  if (length(s)) {
    s<-sort(s)#-----------------------------------
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
            
            
            // The color functions: in this example I'm coloring all the convex hulls at the same layer the same to more easily see the result.
		var color = d3.scale.category20();
              color(0);
              color(1);
              color(2);
              color(3);
              color(4);
              color(5);
              color(6);
              color(7);
              color(8);
              color(9);
			        color(10);
              color(11);
              color(12);
              color(13);
              color(14);
              color(15);
              color(16);
              color(17);
              color(18);
              color(19);
          ",sep="")), file = fileConn)
  
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

  if(length(nodes_with_NA_groups)>0){
    for (i in 1:length(nodes_with_NA_groups))
    {
      members_with_NA_groups[nrow(members_with_NA_groups)+1,1] <- nodes_with_NA_groups[i]
    }
    members_with_NA_groups<-unique(members_with_NA_groups)
  }
  
  #---------------------------------#
  if(layouts_with_virtual_nodes==T){
    node_name <- unique(members_with_NA_groups$id)
    node_name_links <- unique(members_with_NA_groups$id)
  }else{
    node_name<-names(V(g))
    node_name_links<-names(V(g))
  }
  #--------------------------------#

  minx<-min(lay[,1])
  maxx<-max(lay[,1])
  miny<-min(lay[,2])
  maxy<-max(lay[,2])
  
  
  #### Expressions ####
  
  if (!is.null(getStoredExpressionChoices())){
    expression<-fetchFirstSelectedStoredExpression()
    colnames(expression) <- c("id", "color")
    express_order<- as.data.frame(members_with_NA_groups)
    express_order<- as.data.frame(unique(express_order$id))
    colnames(express_order) <- "id"
    expression<-left_join(express_order, expression, by = "id")
    expression$color<- as.character(expression$color)
    expression$color[which(expression$color=="blue")] <- "0"
    expression$color[which(expression$color=="yellow")] <- "16"
    expression$color[which(expression$color=="orange")] <- "2"
    expression$color[which(expression$color=="green")] <- "4"
    expression$color[which(expression$color=="red")] <- "6"
    expression$color[which(expression$color=="purple")] <- "8"
    expression$color[which(expression$color=="gray")] <- "15"
    expression$color[which(is.na(expression$color))] <- "15"
  }
  
  if (is.null(getStoredExpressionChoices())){
      expression<- as.data.frame(members_with_NA_groups)
      expression<- as.data.frame(unique(expression$id))
      expression$color <- rep(c("15"))
      colnames(expression) <- c("id", "color")
  }
  

  ###################
  zoom_slider<-TRUE
  max_allowed_scale<-1
  
  new_g<- get.edgelist(g)
  new_nodes<-unique(union(new_g[,1], new_g[,2]))

  df1<- data.frame(V1= column1)
  df2<- data.frame(V1= column2)
  
  unique_nodes_network<- full_join(df1,df2)
  
  annotations2<- as.character(annoation_graph[,2])
  genes_tmp <- strsplit(annotations2, ",")
  
  unique_nodes_annotations<- as.data.frame(unique(unlist(genes_tmp)))
  colnames(unique_nodes_annotations)<- "V1"
  
  merged<- full_join(unique_nodes_network, unique_nodes_annotations)
  
  words_to_be_removed<- anti_join(merged,unique_nodes_network)

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
      
      if(some_labels==T)
      if(x!=number_of_groups)
      if(!(node_name[i]  %in% selected_genes))
        node_name[i]<-""
      
      if(show_labels == F)
        node_name<-rep("", length(node_name))
      if(expression_colors == T){
    cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20 , ", 'y':", coor_y*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20, ", 'fixed': true, \"color_value\":", expression$color[i], "},\n",sep="")), file = fileConn)
      }
      
      if(expression_colors == F){
    cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20 , ", 'y':", coor_y*scaling_coordinates_convex()-100*scaling_coordinates_convex()+20, ", 'fixed': true, \"color_value\":", 15, "},\n",sep="")), file = fileConn)
      }
    }
  }#if zoom_slider
  else
  {
    for (i in 1:length(new_nodes)){
      coor_x<-mapper(lay[i,1], minx, maxx, 100, 800)
      coor_y<-mapper(lay[i,2], miny, maxy, 100, 800)

   if(length(s)!=6)
      if(!(node_name[i]  %in% selected_genes_with_NAs) )
        node_name[i]<-""
      
      if(show_labels == F)
        node_name<-""
      
      if(expression_colors == T){
        cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20 , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20, ", 'fixed': true, \"color_value\":", expression$color[i], "},\n",sep="")), file = fileConn)
      }
      
      if(expression_colors == F){
        cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":",scaling_nodes_convex(), ",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20 , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20, ", 'fixed': true, \"color_value\":", 15, "},\n",sep="")), file = fileConn)
      }
    }
  }
  
  
  
  cat(sprintf("],
  \"links\":[\n"), file= fileConn)
  for (i in 1:nrowdat){
    cat(sprintf(paste("{\"source\":", which(node_name_links %in% dataset1[i,1])-1, ",\"target\":", which(node_name_links %in% dataset1[i,2])-1, ",\"value\":1},\n",sep="")), file = fileConn
    )}
  
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

  ########################################
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
	  .style(\"fill\", function(d) { return color(d.color_value); })
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
