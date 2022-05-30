################################################################################
##--------------------#FUZZY COGNITIVE MAPS FUNCTIONS#------------------------##
################################################################################


# ACTIVATION VECTOR -- actvector()
################################################################################

#' Create Activation Vector (actvector)
#'
#' @description Function that extracts a column vector from a RepGrid and
#' standardises it so that it can be used as an activation vector in a Fuzzy
#' Cognitive Map.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param col.element column number corresponding to the element from which the
#' activation vector is extracted.
#'
#' @return Returns a vector containing the standardised weights for each of the
#' constructs associated an element.
#'
#' @import OpenRepGrid
#'
#' @export


actvector <- function(grid, col.element = 1){

  vector <- getRatingLayer(grid)[,col.element]                                  # extract vector from de RepGrid.

  result <- (vector - getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)         # Standardise vector into a interval [-1,1].

  return(result)
}
################################################################################

# FCM INFERENCE -- fcminfer()
################################################################################

#' inference of simulated future scenarios -- fcminfer()
#'
#' @description Function to infer simulated future scenarios from a RepGrid and
#' an Impgrid.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param act.vec Activation vector created via \code{\link{actvector}} function.
#' The default vector is the first element of the RepGrid.
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. By default the last column of the RepGrid is set.
#'
#' @param infer Propagation function for scenario inference. More information in
#' ?\code{\link{propfunctions}}.
#'
#' @param thr Threshold function for scenario inference. More information in
#' ?\code{\link{thrfunctions}}.
#'
#' @param lambda Lambda value of the threshold function. Only applicable in
#' sigmoidal or hyperbolic tangent threshold function
#'
#' @param iter Number of iterations to infer.
#'
#' @return Return a list with two entries. The $values entry contains in rows
#' each of the scenario vectors according to the number of iterations. And the
#' $convergence entry contains the number of the iteration where the
#' Fuzzy Cognitive Map is stabilised.
#'
#' @import OpenRepGrid
#'
#' @export

fcminfer <- function(grid, imp, act.vec = actvector(grid), ideal = dim(grid)[2],
                     infer= "mk", thr= "t", lambda = 1 , iter = 30,
                     e = 0.001, force.conv = FALSE){

  imp_a <- .adaptrepgrid(imp, t = FALSE)                                        # Extract ImpGrid values.

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.data.frame(w.mat)                                                 # Transform implication matrix to a weight matrix.



  result <- .infer(act.vec, weight_mat = w.mat, infer = infer,
                   transform = thr, lambda = lambda, iter = iter, e = e,
                   force.conv = force.conv)      # Apply the infer function.

  return(result)
}
################################################################################

# PERSONAL CONSTRUCTS SYSTEM DYNAMICS PLOT -- pcsd()
################################################################################

#' Personal Constructs System Dynamics plot -- pcsd()
#'
#' @description Interactive line plot of personal constructs system dinamics.
#' Show \code{\link{fcminfer}} values expressed in terms of distance to
#' Ideal-Self for each personal construct across the mathematical iterations.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. By default the last column of the RepGrid is set.
#'
#' @param ... This function inherits all the parameters from the function
#' \code{\link{fcminfer}}
#'
#' @return Interactive plot created with plotly.
#'
#' @import plotly
#'
#' @export

pcsd <- function(grid,imp,ideal=dim(grid)[2],...){


  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extract constructs names.
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  iter <- fcminfer(grid,imp,iter=60,force.conv = TRUE)$convergence                                # Save convergence value.

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Create a matrix with Ideal-Self values repeated by rows.
                        nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Apply fcminfer function.

  x <- c(0:(iter-1))
  y <- c(0:length(poles))
  y <- as.character(y)
  df <- data.frame(x, abs(res - ideal.matrix) / 2)                              # Dataframe with the standardised distances between self-now and ideal-self.
  colnames(df) <- y

  fig <- plotly::plot_ly(df, x = ~x, y = df[,2], name = poles[1],
                         type = 'scatter',
                        mode = 'lines+markers',line = list(shape = "spline"))   # Build PCSD with plotly.

  for (n in 3:(length(poles)+1)) {
  fig <- fig %>% plotly::add_trace(y = df[,n], name = poles[n-1],
                                   mode = 'lines+markers'
                                   ,line = list(shape = "spline"))
 }
  fig <- fig %>% plotly::layout(
                        xaxis = list(
                          title = "ITERATIONS"
                          ),
                        yaxis = list(
                          title = "DISTANCE TO IDEAL SELF",
                          range = c(-0.05,1.05)
                          )
                        )
  fig <- fig %>% plotly::layout(legend=list(
                          title=list(text='<b>PERSONAL CONSTRUCTS</b>')
                          )
                        )

  fig                                                                           # Run the results.
}
################################################################################

# FUZZY COGNITIVE MAP DIGRAPH -- fcmdigraph()
################################################################################

#' Fuzzy Cognitive Map Digraph -- fcmdigraph()
#'
#' @description This function draws a digraph of the Fuzzy Cognitive Map of an
#' individual's construct system using a Repgrid and Impgrid.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param results Iteration matrix calculated with
#' \code{\link{fcminfer}}. By default they are calculated with the Self-Now
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. The default is the last column of the RepGrid.
#'
#' @param niter Scenario vector to be represented in the digraph expressed by
#' the row number in the iteration matrix. The default is the first row.
#'
#' @param layout Layout to represent the FCM digraph. More information about
#' layouts in ?\code{\link{digraphlayouts}}.
#'
#' @param edge.width Edge width scalar.
#'
#' @param vertex.size Vertex size scalar.
#'
#' @param legend Draws a legend if TRUE.
#'
#' @return Returns a graphical representation of a digraph of a FCM
#'
#'
#' @importFrom igraph graph.adjacency
#' @importFrom igraph E<-
#' @importFrom igraph V<-
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom igraph add_layout_
#' @importFrom igraph plot.igraph
#' @import OpenRepGrid
#'
#' @export


fcmdigraph <- function(grid, imp, results = fcminfer(grid,imp)$values,
                     ideal = dim(grid)[2], niter = 1,layout = "graphopt",
                     edge.width = 1.5, vertex.size = 1, legend = FALSE ){

  imp_a <- .adaptrepgrid(imp, t = FALSE)                                        # Extract ImpGrid values.

  lpoles <- getConstructNames(grid)[,1]
  rpoles <- getConstructNames(grid)[,2]                                         # Extract construct names from RepGrid.

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.matrix(w.mat)                                                     # Transform Implication Matrix values to weight Matrix.

  results <- as.numeric(as.data.frame(results)[niter,])                         # Extract scenario vector from user input.


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
  if(getRatingLayer(grid)[,ideal][n] > 4){
    if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
    else{
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
      else{
        if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
      }
    }
  }
    if(getRatingLayer(grid)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] == 4){
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
################################################################################


# 3D FUZZY COGNITVE MAP DIGRAPH -- fcmdigraph3D()
################################################################################

#' 3D Fuzzy Cognitive Digraph -- fcmdigraph3D()
#'
#' @description Function that draws a three-dimensional FCM digraph of an
#' personal construct system using a grid technique and an implication
#' grid. It is represented with a 3D multidimensional scaling.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param results Iterarion matrix calculated with
#' \code{\link{fcminfer}}. By default they are calculated with the Self-Now
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. The default is the last column of the RepGrid.
#'
#' @param niter Scenario vector to be represented in the digraph expressed by
#' the row number in the iteration matrix. The default is the first row.
#'
#' @param edge.width Edge width scalar.
#'
#' @param vertex.size Vertex size scalar.
#'
#' @return Returns a rgl output of a FCM digraph in a 3D multidimensional
#' scaling.
#'
#' @importFrom igraph graph.adjacency
#' @importFrom igraph E<-
#' @importFrom igraph V<-
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom igraph add_layout_
#' @importFrom igraph plot.igraph
#' @importFrom igraph as_tree
#' @importFrom igraph in_circle
#' @importFrom igraph with_graphopt
#' @importFrom igraph with_mds
#' @importFrom igraph on_grid
#' @importFrom igraph layout_with_mds
#' @importFrom igraph rglplot

#'
#' @import OpenRepGrid
#'
#' @export
#'

fcmdigraph3D <- function(grid, imp, results = fcminfer(grid,imp)$values,
                       ideal = dim(grid)[2], niter=1, edge.width=2,
                       vertex.size =1){

  imp_a <- .adaptrepgrid(imp, t = FALSE)                                        # Extract ImpGrid values.

  lpoles <- getConstructNames(grid)[,1]
  rpoles <- getConstructNames(grid)[,2]                                         # Extract construct names.

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.matrix(w.mat)                                                     # Transform Implication Matrix in a Weight Matrix

  results <- as.numeric(as.data.frame(results)[niter,])                         # Extract scenario vector from user input.

                                                                                # Orient the weight matrix according the vertex status.
  n <- 1                                                                        # This is for change the colour of the edges depending on vertex status.
  for (integer in results) {
    if(integer != 0){
      integer.value <- integer / abs(integer)
      w.mat[,n] <- w.mat[,n] * integer.value
      w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }

  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Initial empty network.

  V(graph.map)$size <- 1

  n <- 1                                                                        # Edge colour.
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
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

  V(graph.map)$color <- "black"                                                 # Vertex colour.
  n <- 1
  for (pole.vertex in results) {
    if(getRatingLayer(grid)[,ideal][n] > 4){
      if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] == 4){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }

  E(graph.map)$color <- "black"                                                 # Edge colour.
  n <- 1
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
    n <- n + 1
  }

  V(graph.map)$label.cex <- 0.75                                                # Final details.
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"
  V(graph.map)$label.dist <- 1.5

  L <- layout_with_mds(graph.map,dim=3)                                         # Set network layout.

  rglplot(graph.map,layout = L)                                                 # Run digraph with rgl.
}
################################################################################


# IDEAL FUZZY COGNITIVE MAP DIGRAPH -- idealdigraph()
################################################################################
#' Ideal Map Digraph -- idealdigraph()
#'
#' @description This function draws an ideal FCM digraph allows to see
#' implications between Ideal-Self poles.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param inc Shows only inconsistencies (inverse relationships) if TRUE.
#' Default is FALSE.
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. The default is the last column of the RepGrid.
#'
#' @param layout Layout to represent the FCM digraph. More information about
#' layouts in ?\code{\link{digraphlayouts}}.
#'
#' @param edge.width Edge width scalar.
#'
#' @param vertex.size Vertex size scalar.
#'
#' @param legend Draws a legend if TRUE. Default is FALSE.
#'
#' @return Returns Ideal FCM digraph.
#'
#'
#' @importFrom igraph graph.adjacency
#' @importFrom igraph E<-
#' @importFrom igraph V<-
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom igraph add_layout_
#' @importFrom igraph plot.igraph
#' @importFrom igraph as_tree
#' @importFrom igraph in_circle
#' @importFrom igraph with_graphopt
#' @importFrom igraph with_mds
#' @importFrom igraph on_grid
#' @import OpenRepGrid
#'
#' @export

idealdigraph <- function(grid,imp, ideal = dim(grid)[2], inc = FALSE,
                         layout ="circle", edge.width = 1, vertex.size = 1,
                         legend = FALSE){

  act.vector <- actvector(grid,col.element = ideal)
  ideal.results <- fcminfer(grid,imp,act.vector)$values

  imp_a <- .adaptrepgrid(imp, t = FALSE)                                        # Extract ImpGrid values.

  lpoles <- getConstructNames(grid)[,1]                                         # Extract construct names.
  rpoles <- getConstructNames(grid)[,2]

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.matrix(w.mat)                                                     # Transform Implication Matrix in a Weight Matrix.


  results <- as.numeric(as.data.frame(ideal.results)[1,])                       # Extract scenario vector from user input.

  n <- 1
  for (integer in results) {                                                    # Orient the weight matrix according the vertex status.
    if(integer != 0){                                                           # This is for change the colour of the edges depending on vertex status.
      integer.value <- integer / abs(integer)
      w.mat[,n] <- w.mat[,n] * integer.value
      w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }



  if(inc){
    w.mat[w.mat > 0] <- 0
  }


  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Initial empty network.

  E(graph.map)$color <- "black"
  n <- 1
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )               # Edge colour.
    n <- n + 1
  }

  edge.curved <- rep(0, length(E(graph.map)))
  n <- 1                                                                        # Edge curvature.
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
    if(getRatingLayer(grid)[,ideal][n] > 4){
      if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] == 4){
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

  E(graph.map)$arrow.size <- edge.width * 0.3                                   # Final details.
  V(graph.map)$shape <- "circle"
  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"


  if(layout == "rtcircle"){                                                     # Layouts.
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

  plot.igraph(graph.map, edge.curved = edge.curved)                             # Show the digraph on the screen.

  if(legend){                                                                   # Draw a legend.
    poles <- paste(lpoles,rpoles,sep = " - ")
    poles <- paste(c(1:length(poles)),poles,sep = ". ")
    legend("topright",legend = poles, cex = 0.7,
           title = "Constructos Personales")
  }
}
################################################################################

# ANALISYS REPORT -- fcmreport()
################################################################################


#' GridFCM Analisys Report -- fcmreport())
#'
#' @description A function that exports a complete analysis of a subject using
#' rmarkdown draft
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param name Name of the output file.
#'
#' @param dir Directory where the output file is to be created. Default is the
#' working directory.
#'
#' @param output Work in progress.
#'
#' @param edit Allows editing of the draft if TRUE. Default is FALSE.
#'
#' @return Returns a report with a full GridFCM analysis
#'
#' @import rmarkdown
#' @import flexdashboard
#'
#' @export
#'

fcmreport <- function(grid, imp, name = "report", dir = getwd(), output = "html",
                      edit = FALSE){

  render.grid <- grid                                                           # Set render objects for the draft.
  render.imp <- imp

  file <- paste(name,".Rmd", sep = "")                                          # Set output file name.

  file.remove(file)                                                             # Remove possible residual files from a previous error.
  file.remove("style.css")
  file.remove("gridfcm.png")

if(output == "html"){                                                           # Create HTML draft and render it.
  rmarkdown::draft(name,"report_html", package = "GridFCM",edit=edit)
  rmarkdown::render(file , output_dir = dir)
}
if(output =="shiny"){
  rmarkdown::draft(name,"report_shiny", package = "GridFCM",edit=edit)          # Create Shiny draft and run it (WIP).
  rmarkdown::run(file, shiny_args = list(launch.browser = TRUE))
}
if(output =="clean"){
  rmarkdown::draft(name,"clean_document", package = "GridFCM",edit=edit)        # (Inner draft) simplified html version.
  rmarkdown::render(file , output_dir = dir)
}
  file.remove(file)
  file.remove("style.css")
  file.remove("gridfcm.png")                                                    # Remove temp files
}
