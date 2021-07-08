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
#' @param method Metodo que se ha seguido a la hora de aplicar la rejilla de
#' implicaciones. Debe tomar el valor "hinkle" si se utiliza el método de Hinkle
#' y "fransella" en el caso del método de fransella.
#'
#' @return Devuelve una matriz que contiene los datos de la matriz de
#' implicaciones
#'
#' @examples
#'
#' @export
#'
importIMP <- function(path, method="hinkle"){
  xls <- readxl::read_excel(path)
  xls <- xls[,-dim(xls)[2]]
  xls <- xls[,-1]
  rownames(xls) <- colnames(xls)
  result <- t(xls)
  return(result)
}
################################################################################

# IMPORTAR TÉCNICA DE REJILLA
################################################################################

#' Importar Técnica de Rejilla (importGRID)
#'
#' @description Función que permite leer una técnica de rejilla contenida
#' dentro de un archivo de excel.
#'
#' @param path Ruta donde se encuentra el archivo de excel que contiene la
#' la técnica de rejilla.
#'
#' @return Devuelve un objeto S3 RepGrid del paquete OpenRepGrid que contiene la
#' información de la técnica de rejilla.
#'
#' @examples
#'
#' @export

importGRID <- function(path, ...){
  grid <- importExcel(path,...)
  grid <- alignByIdeal(grid,dim(grid)[2])
  grid <- .alignbyself(grid)
  return(grid)
}

