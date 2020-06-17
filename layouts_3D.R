layouts_3D<-c(
  "Fruchterman-Reingold"="Fructerman\tlayout_with_fr(igraph, dim=3)",
  "Random"="Random\tlayout.random(igraph, dim=3)",
  "Kamada-Kawai"="Kamada-Kawai\tlayout.kamada.kawai(igraph, dim=3)",
  # "Reingold-Tilford"="Reingold-Tilford\tlayout.reingold.tilford(igraph, dim=3)",
  # "Lgl"="Lgl\tlayout.lgl(igraph, dim=3)",
  # "Graphopt"="Graphopt\tlayout.graphopt(igraph, dim=3)",
  # "Gem"="Graphopt\tlayout.gem(igraph, dim=3)",
  "Grid"="Grid\tlayout.grid(igraph, dim=3)"
)

selected_layouts=c(
  "Fructerman\tlayout_with_fr(igraph, dim=3)"
)
