################################################################################
#------------------------------INDEX FUNCTIONS---------------------------------#
################################################################################

# FUZZY COGNITIVE MAP DENSITY -- density_index()
################################################################################

#' Fuzzy Cognitive Map density -- density_index()
#'
#' @description Function used to calculate the density of edges of the
#' calculated digraph of the impgrid
#'
#' @param imp  Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#'
#' @return Returns a value from 0 to 1 representing the ratio of the number of
#' edges in the graph over the maximum number of possible edges.
#'
#' @import OpenRepGrid
#'
#' @export

density_index <- function(imp){

  imp.a<- .adaptrepgrid(imp, t = FALSE)                                         # Transform ImpGrid into a operative matrix

  n <- ncol(imp.a)

  result <- sum(degree_index(imp)$Outputs)/(n*(n-1))                            # divide the number of edges by the number of possible edges

  return(result)
}
################################################################################


# DEGREE INDEX CENTRALITY -- degree_index()
################################################################################

#' Degree Index -- degree_index()
#'
#' @description Function to calculate the centrality of the constructs.
#' In this case, centrality is understood as the degree of connection that each
#' construct maintains with the rest, i.e. the number of links for each vertex.
#'
#' @param imp  Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param method Method for calculating centrality. You can use the simple
#' method with "simple", normalized with "norm", weighted with "weigth",
#' normalized weighted with "wnorm" and the ego density method with "ego".
#' Default is Simple Method.
#'
#' @return Returns a list with the centrality data by construct and separated by
#'  input degree, output degree and total degree (in and out).
#'
#' @import OpenRepGrid
#'
#' @export

degree_index <- function(imp, method="simple"){


  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                             # Extract construct names
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  result <- list()                                                              # Create empty list
  imp_a <- .adaptrepgrid(imp, t = FALSE)
  N <- dim(imp_a)[1]

  if(method == "simple" | method == "norm" | method == "ego"){                  # Simple method-----------------------------------------

    imp.grid <- imp_a/imp_a
    imp.grid[is.nan(imp.grid)] <- 0                                               # Convert all the weights in 1

    Cout <- rowSums(imp.grid)
    Cin <- colSums(imp.grid)                                                      # Sum by rows and columns to find the output and input values
  }

  if(method == "weight" | method == "wnorm"){                                   # Weight method----------------------------------------

    imp.grid <- .weightmatrix(imp_a)                                              # Transform ImpGrid into weight matrix

    Cout <- rowSums(abs(imp.grid))
    Cin <- colSums(abs(imp.grid))                                                 # Sum by rows and columns to find the output and input values
  }

  if(method == "norm" | method == "wnorm"){                                     # Standardized method--------------------------------------

    Cout <- Cout/(N-1)
    Cin <- Cin/(N-1)                                                              # Divide output and input values by maximum possible degree
  }

  if(method == "ego"){                                                          # Ego density method-------------------------------------

    Cout <- Cout/(N*(N-1))
    Cin <- Cin/(N*(N-1))                                                         # Divide output and input values by maximum possible number of edges
  names(Cout) <- poles
  names(Cin) <- poles
  }

  result$Outputs <- Cout
  result$Inputs <- Cin
  result$All <- Cout + Cin                                                      # Write the values in the list and return that list

  return(result)
}
################################################################################

# DISTANCE MATRIX -- dismatrix()
################################################################################

#' Distance Matrix -- dismatrix()
#'
#' @description Function that calculates the shortest distance between each of
#' the pairs of digraph constructions.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function
#'
#' @param mode Method to calculate the distances depending on the direction of
#' the edges.With "out" we calculate them respecting the direction of the edges,
#' "in" through the inverse of the direction of the edges and "all" without
#' taking into account the direction.
#'
#' @return Returns the digraph distance matrix. Matrix that contains the
#' distances of the shortest paths from one construct to another.
#'
#' @export
#'

dismatrix <- function(imp,mode="out"){
  imp_a <- .adaptrepgrid(imp, t = FALSE)

  w.mat <- .weightmatrix(imp_a)                                                 # Transform ImpGrid into weight matrix
  w.mat <- as.matrix(w.mat)

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)            # Use the igraph package to calculate the distances

  result <- igraph::shortest.paths(G, weights = NA,mode = mode)

  return(result)
}
################################################################################

# CLOSENESS CENTRALITY INDEX -- close_index()
################################################################################

#' Closeness index -- close_index()
#'
#' @description Function to calculate the closeness of a construct to the rest
#' of the constructs within the digraph.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param norm If TRUE, the values will be standardized. Default is TRUE.
#'
#' @return Returns a vector with the closeness index for each of the
#' constructs.
#'
#' @export
#'

close_index <- function(imp, norm = TRUE){


  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                             # Extract construct names.
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  dist <- dismatrix(imp)                                                        # Calculate dist matrix.
  N <- dim(dist)[1]

  result <- 1/(colSums(dist))

  if(norm){
    result <- (N-1)/(colSums(dist))                                             # Sum the distance of each construct with the rest and normalize.
  }

  names(result) <- poles                                                        # Name vector's elements.

  return(result)
}
################################################################################

# BETWEENESS CENTRALITY INDEX -- betw_index()
################################################################################

#' Betweeness index -- betw_index()
#'
#' @description Function that calculates the betweenness of each of the
#' constructs. This is the number of times a geodesic path (shortest path)
#' between two other constructs passes through that construct in the digraph.
#'
#' @param imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param norm If TRUE, the values will be standardized. Default is TRUE.
#'
#' @return Returns a vector with the betweeness index for each of the
#' constructs.
#'
#' @export
#'

betw_index <- function(imp,norm=TRUE){

  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                             # Extract name of the constructs.
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  imp_a <- imp
  imp_a <- .adaptrepgrid(imp, t = FALSE)

  w.mat <- .weightmatrix(abs(imp_a))
  w.mat <- as.matrix(w.mat)                                                     # Transform impgrid to weight matrix.

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)

  result <- igraph::betweenness(G,normalized = norm,weights = NA )              # Igraph function to betweeness index.

  names(result) <- poles

  return(result)
}
################################################################################

#  PCSD AUC INDEX -- auc_index()
################################################################################

#' PCSD AUC Index -- auc_index()
#'
#' @description This function calculates the area under the PCSD curve for each
#' construct.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param  imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. By default the last column of the RepGrid is set.
#'
#' @param ... This function inherits all the parameters of \code{\link{pcsd}}
#' function.
#'
#' @report Returns a vector with the AUC index of each construct.
#'
#' @import MESS
#'
#' @export

auc_index <- function(grid, imp, ideal=dim(grid)[2],...){

  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extract name of the constructs.
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(grid,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Create a matrix with Ideal-Self values repeated by rows.
                        nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Apply fcminfer function.
  res <- abs(res - ideal.matrix) / 2

  matrix <- matrix(ncol= length(poles), nrow = 1)

  for (n in 1:length(poles)) {                                                  # Calculate AUC for each construct curve.
    matrix[,n] <- MESS::auc(c(1:iter), res[,n], type = "spline")/iter
  }

  result <- as.vector(matrix)

  names(result) <- poles                                                        # Name de vector's elements.


  return(result)
}
################################################################################

#  PCSD STABILITY INDEX -- stability_index()
################################################################################

#' PCSD Stability Index -- stability_index()
#'
#' @description This function returns the standard deviation for each
#' construct over the mathematical iterations of the PCSD.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param  imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. By default the last column of the RepGrid is set.
#'
#' @param ... This function inherits all the parameters of \code{\link{pcsd}}
#' function.
#'
#' @return Returns a vector with the standard deviation of each of the
#' constructs.
#'
#' @import stats
#'
#' @export

stability_index <- function(grid, imp, ideal=dim(grid)[2],...){


  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extract name of the constructs.
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(grid,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Create a matrix with Ideal-Self values repeated by rows.
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Apply fcminfer function.
  res <- abs(res - ideal.matrix) / 2


  result <- apply(res, 2, sd)                                                   # Calculate SD for each construct.

  names(result) <- poles                                                        # Name de vector's elements.

  return(result)
  }
################################################################################

#  PCSD SUMMARY -- pcsd_summary()
################################################################################

#' PCSD summary -- pcsd_summary()
#'
#' @description This function returns a summary of the PCSD. It informs us the
#' initial and final value of each construct and the difference between them.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param  imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. By default the last column of the RepGrid is set.
#'
#' @param ... This function inherits all the parameters of \code{\link{pcsd}}
#' function.
#'
#' @return Returns a matrix with the PCSD summary.
#'
#'
#' @export

pcsd_summary <- function(grid, imp, ideal=dim(grid)[2],...){



  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extract name of the constructs.
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  iter <- fcminfer(grid,imp,iter=60,...)$convergence                            # Extract convergence of the fcminfer.

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Create a matrix with Ideal-Self values repeated by rows.
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Apply fmcinfer function.
  res <- abs(res - ideal.matrix) / 2



  result <- res[c(1,iter),]                                                     # Extract the first vector and the last vector from the iteration matrix.
  result <- t(result)
  result <- cbind(result, result[,2] - result[,1])                              # Calculate the difference between the first vector and the last one and add it to the results.

  rownames(result) <- poles
  colnames(result) <- c("Initial value", "Final value", "Difference")

  return(result)
}

# PERSONAL CONSTRUCTS SYSTEM DERIVATIVE -- pcsd_derivative()
################################################################################

#' PCSD derivative -- pcsd_derivative()
#'
#' @description This function represents the first derivative for each of the
#' PCSD curves.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param  imp Subject's ImpGrid. It must be an S4 object imported by the
#' \code{\link{importimp}} function.
#'
#' @param ideal Column number representing the position of the Ideal-Self in the
#' RepGrid. By default the last column of the RepGrid is set.
#'
#' @param ... This function inherits all the parameters of \code{\link{pcsd}}
#' function.
#'
#' @return Return a plot create via plotly r-package.
#'
#' @import plotly
#'
#' @export

pcsd_derivative <- function(grid,imp,ideal=dim(grid)[2],...){


  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extract name of the constructs
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(grid,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Create a matrix with Ideal-Self values repeated by rows.
                         nrow = iter, byrow = TRUE)

  res.pre <- fcminfer(grid,imp,iter=iter,...)$values                            # Apply fcminfer function
  res.pre <- abs(res.pre - ideal.matrix) / 2


  x <- c(0:(iter-2))
  y <- c(0:length(poles))

  res <- matrix(ncol = length(poles), nrow = iter - 1)

  for (i in 1:length(poles)) {
    res[,i] <- diff(res.pre[,i])/diff(0:(iter-1))                               # Calculate de diffs
  }

  y <- as.character(y)

  df <- data.frame(x,res)                                                       # Made a dataframe with the results.
  colnames(df) <- y

  fig <- plotly::plot_ly(df, x = ~x, y = df[,2], name = poles[1],
                         type = 'scatter', mode = 'lines+markers',
                         line = list(shape = "spline"))

  for (n in 3:(length(poles)+1)) {
    fig <- fig %>% plotly::add_trace(y = df[,n], name = poles[n-1],
                                     mode = 'lines+markers',
                                     line = list(shape = "spline"))
  }

  fig <- fig %>% plotly::layout(xaxis = list(
                                title = "ITERATIONS"),
                                yaxis = list(
                                title = "DERIVATIVE"))

  fig <- fig %>% plotly::layout(legend=list(
                                title=list(text='<b>PERSONAL CONSTRUCTS</b>')))

  fig                                                                           # Config the plot and run it.
}
################################################################################
