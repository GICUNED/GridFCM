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
WeightMatrix <- function(x,imp,sel.vec = NULL){

  #Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(class(x)[1] == "repgrid"){
    x <- .adaptrepgrid(x)
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
#------------------#Iteraciones del Mapa cognitivo
FuzzyIter <- function(w.mat, act.vec, infer= "mk", act= "t", lambda = 1.5 , iter = 30, graph = TRUE){
  require(fcm)
  require(FCMapper)
  w.mat <- as.data.frame(w.mat)
  result <- fcm.infer(act.vec, weight_mat = w.mat, infer = infer, transform = act, lambda = lambda, iter = iter)

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
#---------------------#Mapa cognitivo

FuzzyMap <- function(w.mat,results, lpoles, rpoles, niter = 30 ){
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
  print(results)
  #Color de los vertices
  V(graph.map)$color <- "black"
  n <- 1
  for (pole.vertex in results) {
    if(pole.vertex < 0){V(graph.map)$color[n] <- "red" }
    else{
    if(pole.vertex > 0){V(graph.map)$color[n] <- "green" }
    else{
    if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }

    }
    }
    n <- n + 1
    print(V(graph.map)$color)
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
    print(V(graph.map)$name)

  }



  #Tamaño de los vertices
  V(graph.map)$size <- 1
  n <- 1
  for (size.vertex in results) {
  size.vertex <- abs(size.vertex)
  V(graph.map)$size[n] <- 40 + size.vertex * 75
  n <- n + 1
  }

  #Dibujar plot
  graph.map <- add_layout_(graph.map,as_star())
  plot(graph.map, edge.curved = edge.curved)
}
