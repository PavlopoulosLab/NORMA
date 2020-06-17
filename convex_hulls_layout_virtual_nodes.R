convexInput <- function(){
  set.seed(123)
  g <- fetchFirstSelectedStoredIgraph_annotations_tab()
  if (is.null(g)) 
    return()
  my_network<- as.data.frame(get.edgelist(g))
  my_network<- data.frame(Source = my_network$V1, Target = my_network$V2)
  
  gName <- SelectedStoredNets()$name

  annoation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()
  if (is.null(annoation_graph)) 
    return()
  annotName <- SelectedStoredAnnots()$name
  annoation_graph <- as.data.frame(annoation_graph)
  groups<-annoation_graph

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
  
  #### sort by group as in file #####
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
  names(virt_group_nodes) <- c(letters[1:number_of_groups])
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
  
  nodes_virt[1:length(members_with_NA_groups$id),]<- paste(members_with_NA_groups$id) #replace with the right names from our network
  nodes_virt<- unique(nodes_virt)
  g_virt <- graph_from_data_frame(edges_virt,
                                  directed = FALSE,
                                  vertices = nodes_virt)
  
  # use "auto layout"
  # lay2 <- layout_nicely(g_virt)
  lay <- layout_choices(g_virt, lay)
  
  # remove virtual group nodes from graph
  g_virt <- delete_vertices(g_virt,which(nodes_virt$is_virt == T ))
  
  # remove virtual group nodes' positions from the layout matrix
  tmp<-which(nodes_virt$is_virt == T )
  
  lay <- lay[-tmp, ]
  return(lay)
}


  
