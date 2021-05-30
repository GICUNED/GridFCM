################################################################################
##---------------#FUNCIONES DE MAPAS COGNITIVOS BORROSOS##--------------------##
################################################################################

################################################################################
#------------------------------#Selección de constructos de interés
SelectConstructs <- function(x, sel.vec){

   #Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(class(x)[1] == "repgrid"){
    x <- .adaptrepgrid(x)
  }
  constructs.selected <- sel.vec
  constructs.selected <- as.vector(constructs.selected)
  constructs.selected <- as.numeric(constructs.selected)

    result <- x[constructs.selected,constructs.selected]

    return(result)
}
################################################################################
#------------------------------#Crear matriz de pesos
WeightMatrixP <- function(x,imp,sel.vec = NULL){
test2.matrix <- constructCor(x) * imp
  #Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(class(x)[1] == "repgrid"){
    x <- .adaptrepgrid(x)
  }
  test.matrix <- Pmatrix(x) * imp
  if(any(test.matrix < 0)){
    warning("La matriz de correlaciones parciales y la matriz de implicaciones tienen contradicciones. \n Se usará la matriz de impliciones para calcular la naturaleza de las relaciones \n")
    }

  if(is.null(sel.vec)){
    result <- abs(Pmatrix(x)) * imp
  }else{
    partial.matrix <- Pmatrix(x)[sel.vec,sel.vec]
    result <- abs(partial.matrix) * imp
  }
  return(result)
}
################################################################################
WeightMatrix <- function(x){
  W <- x/3
  return(W)
}
################################################################################
#------------------#Iteraciones del Mapa cognitivo
FuzzyIter <- function(w.mat, act.vec, infer= "mk", thr= "t", lambda = 1 , iter = 30, graph = TRUE){
  require(fcm)
  require(FCMapper)
  w.mat <- as.data.frame(w.mat)
  result <- fcm.infer(act.vec, weight_mat = w.mat, infer = infer, transform = thr, lambda = lambda, iter = iter)

  if(graph){
    require(reshape2)
    require(ggplot2)
    iterations <- as.numeric(rownames(result$values))
    df <- data.frame(iterations, result$values)
    df2 <- melt(df, id="iterations")
    plot(ggplot(data=df2,aes(x=iterations, y=value, group=variable, colour=variable)) +
      theme_bw() + geom_line(size=0.7) + geom_point(size = 3))
  }
  return(result)
}
################################################################################
#---------------------#Mapa cognitivo por pesos

W.FuzzyMap <- function(w.mat,results, lpoles, rpoles, niter = 30, edge.width = 1.5, vertex.size = 1 ){
  require(igraph)
  w.mat <- w.mat
  results <- as.numeric(as.data.frame(results)[niter,])
  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)

  #Color de las aristas.
  E(graph.map)$color <- "black"
  n <- 1
  for (weight in E(graph.map)$weight) {
  E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
  n <- n + 1
  }

  #Curvatura de las aristas.
  edge.curved = rep(0, length(E(graph.map)))
  n <- 1
 for (N in 1:dim(w.mat)[1]) {
 for (M in 1:dim(w.mat)[1]) {
 if(w.mat[M,N] != 0 && w.mat[N,M] != 0){
   edge.curved[n] <- 0.25
 }
   if(w.mat[N,M] != 0){
     n <- n + 1

 }
 }
 }
  #Tamaño de las aristas.
  n <- 1
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[N,M] != 0){
        E(graph.map)$width[n] <- abs(edge.width * w.mat[N,M])
        n <- n + 1
      }
    }
  }

  #Color de los vertices
  V(graph.map)$color <- "black"
  n <- 1
  for (pole.vertex in results) {
    if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
    else{
    if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
    else{
    if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
    }
    }
    n <- n + 1
  }

  #Nombre de los vertices
  n <- 1
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"-",rpoles[n])}
      }
    }
    n <- n + 1
  }

  #Tamaño de los vertices
  V(graph.map)$size <- 1
  n <- 1
  for (size.vertex in results) {
  size.vertex <- abs(size.vertex)
  V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
  n <- n + 1
  }

  #Dibujar plot
  E(graph.map)$arrow.size <- 0.3
  V(graph.map)$shape <- "circle"
  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"
  graph.map <- add_layout_(graph.map,as_tree(circular = TRUE))
  plot.igraph(graph.map, edge.curved = edge.curved)
}

################################################################################
#---------------------#Mapa cognitivo

FuzzyMap <- function(x, imp, results, col.ideal = dim(x)[2], niter = 30, layout = "rtcircle", edge.width = 1.5, vertex.size = 1, legend = FALSE ){
  require(igraph)

  lpoles <- getConstructNames(x)[,1]
  rpoles <- getConstructNames(x)[,2]

  w.mat <- WeightMatrix(imp)
  w.mat <- as.matrix(w.mat)
  results <- as.numeric(as.data.frame(results)[niter,])

  n <- 1
  for (integer in results) {
    integer.value <- integer / abs(integer)
    w.mat[,n] <- w.mat[,n] * integer.value
    w.mat[n,] <- w.mat[n,] * integer.value
    n <- n + 1
  }

  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)

  #Color de las aristas.
  E(graph.map)$color <- "black"
  n <- 1
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
    n <- n + 1
  }

  #Curvatura de las aristas.
  edge.curved = rep(0, length(E(graph.map)))
  n <- 1
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[M,N] != 0 && w.mat[N,M] != 0){
        edge.curved[n] <- 0.25
      }
      if(w.mat[N,M] != 0){
        n <- n + 1

      }
    }
  }
  #Tamaño de las aristas.
  n <- 1
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[N,M] != 0){
        E(graph.map)$width[n] <- abs(edge.width * w.mat[N,M])
        n <- n + 1
      }
    }
  }

  #Color de los vertices
  V(graph.map)$color <- "black"
  n <- 1
  for (pole.vertex in results) {
  if(getRatingLayer(x)[,col.ideal][n] > 4){
    if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
    else{
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
      else{
        if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
      }
    }
  }
    if(getRatingLayer(x)[,col.ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(x)[,col.ideal][n] == 4){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }

  #Nombre de los vertices
  n <- 1
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"-",rpoles[n])}
      }
    }
    n <- n + 1
  }

  #Tamaño de los vertices
  V(graph.map)$size <- 1
  n <- 1
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
    n <- n + 1
  }

  #Retoques finales
  E(graph.map)$arrow.size <- edge.width * 0.3
  V(graph.map)$shape <- "circle"
  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"

  #Layout
  if(layout == "rtcircle"){
   graph.map <- add_layout_(graph.map,as_tree(circular = TRUE))
  }
  if(layout == "tree"){
    graph.map <- add_layout_(graph.map,as_tree())
  }
  if(layout == "circle"){
    graph.map <- add_layout_(graph.map,in_circle())
  }
  if(layout == "graphopt"){
    graph.map <- add_layout_(graph.map,with_graphopt())
  }
  if(layout == "mds"){
    graph.map <- add_layout_(graph.map,with_mds())
  }
if(layout == "grid"){
  graph.map <- add_layout_(graph.map,on_grid())
}

  if(layout == "mds3d"){
    require(rgl)
    options(rgl.printRglwidget = TRUE)
    L <- layout_with_mds(graph.map,dim=3)
   rglplot(graph.map,layout = L)
  rglwidget()
  }else{
  plot.igraph(graph.map, edge.curved = edge.curved)
  }
  #Leyeda
  if(legend){
    poles <- paste(lpoles,rpoles,sep = " - ")
    poles <- paste(c(1:length(poles)),poles,sep = ". ")
    legend("topright",legend = poles, cex = 0.7, title = "Constructos Personales")
  }
}
