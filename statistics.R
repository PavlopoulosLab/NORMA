statistics <- c(
  "Number of Edges" = "Number of Edges\tecount(igraph)",
  "Number of Nodes" = "Number of Nodes\tvcount(igraph)",
  "Density" = "Density\tgraph.density(igraph)",
  "Average path length" = "Average path length\taverage.path.length(igraph)",
  "Clustering Coefficient" = "Clustering Coefficient\ttransitivity(igraph)",
  "Modularity" = "Modularity\tmodularity(igraph,membership(walktrap.community(igraph)))",
  "Average Eccentricity" = "Average Eccentricity\tmean(eccentricity(igraph))",
  "Average number of Neighbors" = "Average number of Neighbors\t(centr_eigen(igraph)$centralization)",
  "Centralization betweenness" = "Centralization.betweenness\tcentralization.betweenness(igraph)$centralization",
  # "Centralization closeness" = "Centralization.closeness\tcentralization.closeness(igraph)$centralization",
  "Centralization degree" = "Centralization.degree\tcentralization.degree(igraph)$centralization"
  )
selected_statistics <- c("Number of Edges" = "Number of Edges\tecount(igraph)")
                         # "Number of Nodes" = "Number of Nodes\tvcount(igraph)")