pie_charts<- function(){
  
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
  
  if(layouts_with_virtual_nodes_pies==T){
  source("pie_charts_layout_virtual_nodes.R", local = T)
  lay<-pie_chartsInput()}
  else{set.seed(123)
    lay <- layout_choices(g, lay)
  }

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
  groupss <- inner_join(members_with_zeros, groupss, by = "id")
  groupss <- data.frame(groupss[,1],groupss[,3])
  groupss <- groupss[!duplicated(groupss[,1]), ]
  colnames(groupss)<- c("V1", "V2")
  groupss <- as.data.frame(groupss)
  
  #---------------------------------#
  if(layouts_with_virtual_nodes_pies==T){
    node_name <- unique(members_with_NA_groups$id)
    node_name_links <- unique(members_with_NA_groups$id)
    groupss_as_charachter<- as.character(groupss$V2)
    
  }else{
    node_name<-names(V(g))
    node_name_links<-names(V(g))
    
    not_virtual_nodes <- groupss[order(match(groupss[,1],node_name)),]
    groupss_as_charachter<- as.character(not_virtual_nodes$V2)
    
  }
  Groupss <- strsplit(groupss_as_charachter, ",")
  #--------------------------------#
  
  
  #### Expressions ####
  
  if (!is.null(getStoredExpressionChoices())){
    expressions_pies<-fetchFirstSelectedStoredExpression()
    colnames(expressions_pies) <- c("id", "color")
    express_order<- as.data.frame(members_with_NA_groups)
    express_order<- as.data.frame(unique(express_order$id))
    colnames(express_order) <- "id"
    expressions_pies<-left_join(express_order, expressions_pies, by = "id")
    expressions_pies$color<- as.character(expressions_pies$color)
    expressions_pies$color[which(expressions_pies$color=="blue")] <- "0"
    expressions_pies$color[which(expressions_pies$color=="yellow")] <- "16"
    expressions_pies$color[which(expressions_pies$color=="orange")] <- "2"
    expressions_pies$color[which(expressions_pies$color=="green")] <- "4"
    expressions_pies$color[which(expressions_pies$color=="red")] <- "6"
    expressions_pies$color[which(expressions_pies$color=="purple")] <- "8"
    expressions_pies$color[which(expressions_pies$color=="gray")] <- "15"
    expressions_pies$color[which(is.na(expressions_pies$color))] <- "15"
  }
  
  if (is.null(getStoredExpressionChoices())){
    expressions_pies<- as.data.frame(members_with_NA_groups)
    expressions_pies<- as.data.frame(unique(expressions_pies$id))
    expressions_pies$color <- rep(c("15"))
    colnames(expressions_pies) <- c("id", "color")
  }
  
  ###################
  
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
    
    if(some_labels_pies==T)
    if(x!=number_of_groups)
      if(!(node_name[i]  %in% selected_genes))
        node_name[i]<-""
    
    if(show_labels_pies == F)
      node_name<-rep("", length(node_name))
    if(expression_colors_pies == T){
    cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20  , ", 'y':", coor_y*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20 , ", 'fixed': true, \"color_value\":", expressions_pies$color[i], ",\"proportions\": [\n",sep="")), file = fileConn)
    }
    if(expression_colors_pies == F){
    cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20  , ", 'y':", coor_y*scaling_coordinates_pies()-100*scaling_coordinates_pies()+20 , ", 'fixed': true, \"color_value\":", 15, ",\"proportions\": [\n",sep="")), file = fileConn)
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
      if(show_labels_pies == F)
        node_name<-rep("", length(node_name))
      if(expression_colors_pies == T){
        cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20  , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20 , ", 'fixed': true, \"color_value\":", expressions_pies$color[i], ",\"proportions\": [\n",sep="")), file = fileConn)
      }
      if(expression_colors_pies == F){
        cat(sprintf(paste("{\"id\":", i-1, ",name:\"", node_name[i],"\",\"propertyValue\":", 3,",'x':", coor_x*max_allowed_scale-100*max_allowed_scale+20  , ", 'y':", coor_y*max_allowed_scale-100*max_allowed_scale+20 , ", 'fixed': true, \"color_value\":", 15, ",\"proportions\": [\n",sep="")), file = fileConn)
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

  cat(sprintf("],\n
						\"links\":[\n"), file = fileConn)
  for (i in 1:nrowdat){
    cat(sprintf(paste("{\"source\":", which(node_name_links %in% dataset1[i,1])-1, ",\"target\":", which(node_name_links %in% dataset1[i,2])-1, "},\n",sep="")), file = fileConn
    )}
  cat(sprintf(
    "]
    };\n "), file = fileConn)
		
  
  cat(sprintf("var width = 10000,
			height = 10000,
			radius = 25,
			color = d3.scale.linear().domain([",sep=""), file = fileConn)
  
  
  ##### Colors ####
  for(i in 0:x){
    cat(sprintf(paste(i, "," ,sep="")), file = fileConn)}
  
  vector_zero<- groupss$V2==0
  length_vector_zero<- sum(vector_zero, na.rm = TRUE)
  
  cat(sprintf("]).range([\"#cccccc\","), file = fileConn)
  for(i in 1:x){
      cat(sprintf(paste( "\"", qual_col_pals[s[i]], "\"," ,sep="")), file = fileConn)
    }
  cat(sprintf("])\n"), file = fileConn)
 
  cat(sprintf("var color_border = d3.scale.category20();
              color_border(0);
              color_border(1);
              color_border(2);
              color_border(3);
              color_border(4);
              color_border(5);
              color_border(6);
              color_border(7);
              color_border(8);
              color_border(9);
			        color_border(10);
              color_border(11);
              color_border(12);
              color_border(13);
              color_border(14);
              color_border(15);
              color_border(16);
              color_border(17);
              color_border(18);
              color_border(19);
              
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
			.attr(\"class\", \"link\");

		var node = svg.selectAll(\".node\")
			.data(graph.nodes)
			.enter().append(\"g\")
			.attr(\"class\", \"node\")
			.style(\"stroke\", function(d) { return color_border(d.color_value); })
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