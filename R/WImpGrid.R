### WEIGHTED IMPGRID FUNCTIONS ###

# Lineal Threshold Function -----------------------------------------------

#' Linear threshold Function
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

lineal.thr <- function(x){

  if(x <= -1){ result <- -1}
  if(-1 < x && x < 1){ result <- x}
  if(x >= 1){ result <- 1}

  return(result)
}

# Import Weigthed ImpGrid -------------------------------------------------

#' Import Weighted ImpGrid
#'
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples


importwimp <- function(path,...){

  wimp <- list()
  class(wimp) <- "WeigthedImpGrid"

  xlsx <- read_excel(file.choose())

  n.constructs <- dim(xlsx)[1]

  # Scale -------------------------------------------------------------------


  scale.min <- as.numeric(names(xlsx)[1])
  scale.max <- as.numeric(names(xlsx)[n.constructs + 3])
  scale.center <- (scale.min + scale.max)/2

  scale <- c(scale.min,scale.max)
  names(scale) <- c("min","max")

  wimp$scale <- scale


  # Constructs --------------------------------------------------------------

  left.poles <- as.vector(xlsx[,1])[[1]]
  right.poles <- as.vector(xlsx[,n.constructs + 3])[[1]]

  constructs <- paste(left.poles,"—",right.poles,sep = "")

  wimp$constructs[[1]] <- left.poles
  wimp$constructs[[2]] <- right.poles
  wimp$constructs[[3]] <- constructs

  names(wimp[["constructs"]]) <- c("left.poles","right.poles","constructs")


  # Self vector -------------------------------------------------------------

  direct.scores <- as.matrix(xlsx[,1:n.constructs+1])

  direct.self <- diag(direct.scores)

  standarized.self <- (direct.self - (scale.center * rep(1,n.constructs))) / (0.5 * (scale.max - scale.min))

  wimp$self[[1]] <- direct.self
  wimp$self[[2]] <- standarized.self
  names(wimp$self) <- c("direct","standarized")


  # Ideal vector ------------------------------------------------------------

  direct.ideal <- as.vector(xlsx[,n.constructs + 2])[[1]]
  standarized.ideal <- (direct.ideal - (scale.center * rep(1,n.constructs))) / (0.5 * (scale.max - scale.min))

  wimp$ideal[[1]] <- direct.ideal
  wimp$ideal[[2]] <- standarized.ideal
  names(wimp$ideal) <- c("direct","standarized")


  # Hypothetical vector -----------------------------------------------------

  standarized.hypothetical <- rep(0,n.constructs)

  n <- 1
  for (i in standarized.self) {

    if(i != 0){
      standarized.hypothetical[n] <- standarized.self[n] / (-1 * abs(standarized.self[n]))
    }
    if(i == 0 && standarized.ideal != 0 ){
      standarized.hypothetical[n] <- standarized.ideal[n] / abs(standarized.ideal[n])
    }
    if(i == 0 && standarized.ideal == 0){
      standarized.hypothetical[n] <- 1
    }
    n <- n + 1
  }

  direct.hypothetical <- (scale.center * rep(1,n.constructs)) + (standarized.hypothetical * (0.5 * (scale.max - scale.min)))

  wimp$hypothetical[[1]] <- direct.hypothetical
  wimp$hypothetical[[2]] <- standarized.hypothetical
  names(wimp$hypothetical) <- c("direct","standarized")


  # Scores ------------------------------------------------------------------

  imp.matrix <- t((direct.scores - (scale.center * matrix(rep(1,n.constructs * n.constructs),ncol = n.constructs))) / (0.5 * (scale.max - scale.min)))

  num.weight.matrix <- imp.matrix - matrix(standarized.self,nrow = n.constructs,ncol = n.constructs,byrow = TRUE)
  den.weigth.matrix <- matrix(standarized.hypothetical,nrow = n.constructs,ncol = n.constructs) - matrix(standarized.self,nrow = n.constructs,ncol = n.constructs)
  weight.matrix <- num.weight.matrix / den.weigth.matrix

  wimp$scores[[1]] <- direct.scores
  wimp$scores[[2]] <- imp.matrix
  wimp$scores[[3]] <- weight.matrix
  names(wimp$scores) <- c("direct","implications","weights")



  # Function return ---------------------------------------------------------


  return(wimp)
}

# Scenario Matrix -----------------------------------------------------------

#' Scenario Matrix
#'
#' @param wimp
#' @param act.vector
#' @param max.iter
#' @param e
#' @param stop.iter
#' @param force.convergence
#' @param quiet
#'
#' @return
#' @export
#'
#' @examples

scenariomatrix <- function(wimp, act.vector, max.iter = 30, e = 0.0001, stop.iter = 3, force.convergence = FALSE, quiet = FALSE ){

  if(!inherits(wimp,"WeigthedImpGrid")){
    stop("La matriz de pesos debe de ser de la clase WeightedImpGrid")
  }
  if( ncol(wimp[[6]][[3]]) != length(act.vector)){
    stop("El vector de activación tiene una dimensión incompatible con la matriz de pesos")
  }

  scene.matrix <- t(matrix(wimp[[3]][[2]]))

  n <- 1
  i <- 0

  while(n <= max.iter && i <= stop.iter){

    next.iter <- scene.matrix[n,] + t(act.vector)
    next.iter <- sapply(next.iter, lineal.thr)

    delta.iter <- next.iter - scene.matrix[n,]
    scene.matrix <- rbind(scene.matrix, next.iter)

    act.vector <- t(wimp[[6]][[3]]) %*% delta.iter

    e.iter <- mean(next.iter - scene.matrix[n,])

    if(e.iter < e){
      i <- i + 1
    }else{
      i <- 0
    }
    n <- n + 1
  }
  rownames(scene.matrix) <- paste("iter", 0:(n-1))
  colnames(scene.matrix) <- wimp[[2]][[3]]

  if(n < max.iter){
    convergence <- n - (stop.iter + 1)
  }else{
    if(force.convergence){
      convergence <- n
    }else{
      convergence <- NA
    }
    if(!quiet){cat("\n\nConvergencia no alcanzada, prueba un mayor valor para max.iter\n\n")}
  }

  scene.list <- list()

  scene.list$values <- scene.matrix
  scene.list$convergence <- convergence


  return(scene.list)
}


# Digraph -----------------------------------------------------------------

#' Wimp digraph
#'
#' @param wimp
#' @param act.vector
#' @param scene.matrix
#' @param niter
#' @param layout
#' @param edge.width
#' @param vertex.size
#' @param legend
#'
#' @return
#' @export
#'
#' @examples

selfdigraph <- function(wimp, act.vector, scene.matrix = "default", niter = 1,layout = "graphopt", edge.width = 1.5, vertex.size = 1, legend = FALSE ){

  lpoles <- wimp[[2]][[1]]
  rpoles <- wimp[[2]][[2]]                                                     # Extract construct names from RepGrid.

  w.mat <- wimp[[6]][[3]]

  if(is.character(scene.matrix)){
    results <- scenariomatrix(wimp,act.vector)$values[niter,]
  }else{
  results <- results$values[niter,]                                        # Extract scenario vector from user input.
  }

  n <- 1                                                                         # Orient the weight matrix according the vertex status.
                                                                                # This is for change the colour of the edges depending on vertex status.
  for (integer in results) {
    if(integer != 0){
      integer.value <- integer / abs(integer)
      w.mat[,n] <- w.mat[,n] * integer.value
      w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }


  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Initial empty network.

  E(graph.map)$color <- "black"
  n <- 1                                                                        # Edge colour.
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
    n <- n + 1
  }

  edge.curved <- rep(0, length(E(graph.map)))                                   # Edge curvature.
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

  n <- 1                                                                        # Edge width.
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[N,M] != 0){
        E(graph.map)$width[n] <- abs(edge.width * w.mat[N,M])
        n <- n + 1
      }
    }
  }

  V(graph.map)$color <- "black"
  n <- 1                                                                        # Vertex colour.
  for (pole.vertex in results) {
    if(wimp[[4]][[2]][n] > 0){
      if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(wimp[[4]][[2]][n] < 0){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(wimp[[4]][[2]][n] == 0){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }

  n <- 1                                                                        # Vertex name.
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"-",
                                                                rpoles[n],sep =
                                                                  " ")}
      }
    }
    n <- n + 1
  }

  V(graph.map)$size <- 1
  n <- 1                                                                        # Vertex size.
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
    n <- n + 1
  }
  # Final details.
  E(graph.map)$arrow.size <- edge.width * 0.3
  V(graph.map)$shape <- "circle"
  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"

  # Layouts.
  if(layout == "rtcircle"){
    graph.map <- add_layout_(graph.map,as_tree(circular = TRUE, mode = "out"))
  }
  if(layout == "tree"){
    graph.map <- add_layout_(graph.map,as_tree())
  }
  if(layout == "circle"){
    graph.map <- add_layout_(graph.map,in_circle())
  }
  if(layout == "graphopt"){
    set.seed(3394)
    matrix.seed <- matrix(rnorm(2 * dim(grid)[1]), ncol = 2)

    graph.map <- add_layout_(graph.map,with_graphopt(start = matrix.seed))
  }
  if(layout == "mds"){
    graph.map <- add_layout_(graph.map,with_mds())
  }
  if(layout == "grid"){
    graph.map <- add_layout_(graph.map,on_grid())
  }

  plot.igraph(graph.map, edge.curved = edge.curved)                             # Show the digraph on the screen.

  if(legend){                                                                   # Draw legend.
    poles <- paste(lpoles,rpoles,sep = " - ")
    poles <- paste(c(1:length(poles)),poles,sep = ". ")
    legend("topright",legend = poles, cex = 0.7,
           title = "Constructos Personales")
  }
}
