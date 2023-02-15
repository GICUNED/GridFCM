# SCENARIO SIMULATION -- scenariosimulation()
################################################################################

#' inference of simulated future scenarios -- scsim()
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

scsim <- function(grid, imp, init.vec = actvector(grid), act.vec, iter = 30,
                      e = 0.001, force.conv = FALSE){

  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  imp_a <- .adaptrepgrid(imp, t = TRUE)

  w.mat <- .weightmatrix(imp_a)

  iteration.matrix <- t(matrix(as.numeric(init.vec)))

  diag(w.mat) <- 0

  i <- 1
  while(i <= iter) {

    iter.vec <- iteration.matrix[i,]

    changes <- w.mat %*% act.vec

    iteration <- t(iter.vec + changes)

    iteration <- tanh(iteration)

    iteration.matrix <- rbind(iteration.matrix, iteration)

    act.vec <- tanh(changes)

    i <- i + 1


  }

  colnames(iteration.matrix) <- poles
  rownames(iteration.matrix) <- 0:iter

  outlist <- list()

  outlist$iterations <- iteration.matrix

  convergence <- NA
  diff.matrix <- iteration.matrix[2:iter,] - iteration.matrix[1:iter-1,]
  exit <- 0
  n <- 1
  while(exit < 3 && n < iter){

    row <- diff.matrix[n,]
    mean.row <- abs(sum(row)/length(row))

    if(mean.row <= e){
      exit <- exit + 1
      n <- n + 1
      convergence <- n
    }else{
      n <- n+1
      exit <- 0
      convergence <- NA
    }
  }

  if(force.conv && is.na(convergence)){
    convergence <- iter
  }

  outlist$convergence <- convergence
  return(outlist)
}

# IMPORT WEIGTHED IMPGRID -- wimpimport()
################################################################################

#' Import Weigthed ImpGrid -- wimpimport()
#'
#' @description Function used to read Weighted ImpGrid inside an xlsx file. For the
#' creation of the ImpGrid template you can use the function
#' \code{\link{templateimp}}
#'
#' @param path xlsx file path
#'
#' @param ... This function inherits all the parameters of function
#' \code{\link{importExcel}} from OpenRepGrid package.
#'
#' @return Returns an S4 repgrid object containing the data from the ImpGrid
#'
#' @export

