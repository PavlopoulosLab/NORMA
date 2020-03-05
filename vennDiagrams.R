vennDiagrams<- function(){
g <- fetchFirstSelectedStoredIgraph_annotations_tab()
if (is.null(g))
  return()
my_network <- as.data.frame(get.edgelist(g))
my_network <- data.frame(from = my_network$V1, to = my_network$V2)

gName <- SelectedStoredNets()$name
annoation_graph <- fetchFirstSelectedStoredGroups2_annotations_tab()

if (is.null(annoation_graph))
  return()
annotName <- SelectedStoredAnnots()$name
annoation_graph <- as.data.frame(annoation_graph)
groups <- annoation_graph
groups <- data.frame(V1 = groups$Annotations,
                     stri_split_fixed(groups$Nodes, ",",  simplify = TRUE))
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

column1 <- my_network$from
column2 <- my_network$to
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
