# IMPORTAR REJILLA DE IMPLICACIONES
###############################################################################
#' Importar Rejilla de Implicaciones (importIMP)
#'
#' @description Función que permite leer una rejilla de implicaciones contenida
#' dentro de un archivo de excel.
#'
#' @param path Ruta donde se encuentra el archivo de excel que contiene la
#' rejilla de implicaciones.
#'
#' @return Devuelve una matriz que contiene los datos de la matriz de
#' implicaciones
#'
#' @examples
#'
#' @export
#'
importIMP <- function(path){
  xls <- readxl::read_excel(path)
  xls <- xls[,-dim(xls)[2]]
  xls <- xls[,-1]
  rownames(xls) <- colnames(xls)
  result <- t(xls)
  return(result)
}
################################################################################

# MATRIZ DE DISTANCIAS
################################################################################
DistanceMatrix <- function(imp){

  w.mat <- WeightMatrix(imp)
  w.mat <- as.matrix(w.mat)

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)

  result <- igraph::shortest.paths(G, weights = NA)

  return(result)
}
