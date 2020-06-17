plotly_trial<- function(){
  
  
  trace0 <- list(
    uid = "bcd52d", 
    line = list(
      color = "rgb(125,125,125)", 
      width = 1
    ), 
    mode = "lines", 
    name = "Trace 0, y", 
    type = "scatter3d", 
    x = c(9,2,	  	NULL,2,3,		NULL,3,4,	   NULL, 1,4,	   NULL, 2,4,	  NULL, 4,5,	 NULL,3,6,	   NULL,5,7,	NULL),
    y = c(5,0.2,	NULL,0.2,0.3, 	NULL,0.3,0.4,  NULL, 0.1,0.4,  NULL, 0.2,0.4, NULL, 0.4,0.5, NULL,0.3,0.6, NULL,0.5,0.7,NULL),
    z = c(1,1,NULL,1,1,	NULL,1,1,NULL, 1,1,NULL, 1,1,NULL,1,2,NULL,1,2,NULL,2,2,NULL),
    hoverinfo = "none"
  )
  
  trace1 <- list(
    mode = "markers", 
    name = "Cluster 0", 
    type = "scatter3d", 
    x = c(9,2,3,4,5,6,7),
    y = c(5,0.2,0.3,0.4,0.5,0.6,0.7),
    z = c(1, 1, 1, 1, 1, 2, 2),
    marker = list(
      size = 2, 
      color = "black"
    )
  )
  trace2 <- list(
    mode = "markers", 
    name = "Cluster 1", 
    type = "scatter3d", 
    x = c(9,4,5,6),
    y = c(5,0.4,0.5,0.6),
    z = c(1, 1, 2, 2),
    marker = list(
      size = 2, 
      color = "black"
    )
  )
  
  trace7 <- list(
    name = "Cluster 0", 
    type = "mesh3d", 
    x = c(9,2,3,4,5,6,7),
    y = c(5,0.2,0.3,0.4,0.5,0.6,0.7),
    z = c(1, 1, 1, 1, 1, 2, 2),
    color = "green", 
    opacity = 0.1, 
    alphahull = 1, 
    showscale = TRUE
  )
  trace8 <- list(
    name = "Cluster 1", 
    type = "mesh3d", 
    x = c(9,4,5,6),
    y = c(5,0.4,0.5,0.6),
    z = c(1, 1, 2, 2),
    color = "blue", 
    opacity = 0.1, 
    alphahull = 1, 
    showscale = TRUE
  )
  
  
  data <- list(trace0, trace1, trace2, trace7, trace8)
  layout <- list(
    scene = list(
      xaxis = list(zeroline = TRUE), 
      yaxis = list(zeroline = TRUE), 
      zaxis = list(zeroline = TRUE)
    ), 
    title = "Interactive Cluster Shapes in 3D"
  )
  p <- plot_ly()
  #lines
  p <- add_trace(p, mode=trace0$mode, name=trace0$name, type=trace0$type, x=trace0$x, y=trace0$y, z=trace0$z, marker=trace0$marker)
  #covex hulls 
  p <- add_trace(p, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, z=trace1$z, marker=trace1$marker)
  p <- add_trace(p, mode=trace2$mode, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, z=trace2$z, marker=trace2$marker)
  p <- add_trace(p, name=trace7$name, type=trace7$type, x=trace7$x, y=trace7$y, z=trace7$z, color=trace7$color, opacity=trace7$opacity, alphahull=trace7$alphahull, showscale=trace7$showscale)
  p <- add_trace(p, name=trace8$name, type=trace8$type, x=trace8$x, y=trace8$y, z=trace8$z, color=trace8$color, opacity=trace8$opacity, alphahull=trace8$alphahull, showscale=trace8$showscale)
  p <- layout(p, scene=layout$scene, title=layout$title)
  p
  return(p)
  
}