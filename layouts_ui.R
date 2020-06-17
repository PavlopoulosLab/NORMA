layouts_ui<-c(
"Fruchterman-Reingold"="Fructerman\tlayout_nicely(igraph, dim=2)",
# "Fruchterman-Reingold"="Fructerman\tlayout.fruchterman.reingold(igraph, dim=2)",
"Random"="Random\tlayout.random(igraph, dim=2)",
"Circle"="Circle\tlayout.circle(igraph)",
"Kamada-Kawai"="Kamada-Kawai\tlayout.kamada.kawai(igraph, dim=2)",
"Reingold-Tilford"="Reingold-Tilford\tlayout.reingold.tilford(igraph)",
"Lgl"="Lgl\tlayout.lgl(igraph)",
"Graphopt"="Graphopt\tlayout.graphopt(igraph)",
"Gem"="Graphopt\tlayout.gem(igraph)",
"Star"="Graphopt\tlayout_as_star(igraph)",
"Grid"="Grid\tlayout.grid(igraph)"
# "Auto"="Auto\tlayout.auto(igraph, dim=2)",
# "SVD"="SVD\tlayout.svd(igraph, d=shortest.paths(igraph))"
# "Nicely"="Nicely\tlayout_nicely(igraph, dim=2)",
# "Tree"="Graphopt\tlayout_as_tree(igraph)",
# "Spring"="Spring\tlayout.spring(igraph)",
# "Fruchterman-Reingold Grid"="Fruchterman Grid\tlayout.fruchterman.reingold.grid(igraph)",
# "Sphere"="Sphere\tlayout.sphere(igraph)",
)

selected_layouts=c(
  "Fructerman\tlayout_nicely(igraph, dim=2)"
  )
