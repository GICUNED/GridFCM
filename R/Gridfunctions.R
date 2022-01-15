################################################################################
##----------------------------#GRID FUNCTIONS##-------------------------------##
################################################################################

# IMPORT IMPGRID --- importimp()
###############################################################################
#' Importar Rejilla de Implicaciones (importimp)
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

importimp <- function(path,...){

  result <- importExcel(path, ...)

  return(result)
}

# IMPORT IMPGRID --- deprecated
###############################################################################

importimp_d <- function(path, method="hinkle"){
  xls <- readxl::read_excel(path)                                               # Leemos el excel que contiene los datos
  xls <- xls[,-dim(xls)[2]]
  xls <- xls[,-1]                                                               # Eliminamos las columnas exteriores

  rownames(xls) <- colnames(xls)                                                # Le damos nombre a las columnas

  result <- t(xls)                                                              # Trasponemos la matriz para que tenga el formato de una matriz de adyacencia
  return(result)
}
################################################################################

# IMPORT IMPGRID -- importgrid()
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

importgrid <- function(path, ...){

  grid <- importExcel(path, ...)                                                # Importamos la rejilla con la función del OpenRepGrid,
  grid <- alignByIdeal(grid,dim(grid)[2])                                       # Orientamos la rejilla según el ideal
  grid <- .alignbyself(grid)                                                    # Orientamos la rejilla según el Yo-Actual
  return(grid)
}
################################################################################

# EXPORT IMPGRID TEMPLATE -- templateimp()
################################################################################

#' Importar Técnica de Rejilla (importGRID)
#'
#' @description Función que crea en el directorio de trabajo un archivo de excel
#' con una plantilla personalizada de la Rejilla de Implicaciones en base a una
#' Tecnica de Rejilla
#'
#' @param x Técnica de Rejilla importada a través de la función
#' \code{\link{importGRID}. Debe de ser un objeto RepGrid.
#'
#' @return Exporta un documento xlsx en el directorio de trabajo asignado de la
#' sesión.
#'
#' @examples
#'
#' @import xlsx
#' @export

templateimp <- function(x,name ="ImpGrid_Template"){
  requireNamespace("xlsx")                                                      # Comprobamos que el sujeto tenga instalado y ejecutado xlsx

  dim <- dim(x)[1]                                                              # Guardamos el número de constructos de la rejilla

  wb <- createWorkbook()
  sh <- createSheet(wb)                                                         # Creamos el documento de trabajo y la hoja de trabajo


  fill.izq <- CellStyle(wb, fill = Fill("#ffff6d","#ffff6d"))
  fill.der <- CellStyle(wb, fill = Fill("#729fcf","#729fcf"))
  fill.imp <- Fill("#77bc65","#77bc65")                                         # Establecemos los colores para las celdas


  dfs <- list(fill.izq,fill.der)
  names(dfs) <- c(1,dim +2)                                                     # Establecemos los estilos de las celdas

  rotate.90 <- Alignment(rotation = 90)                                         # Definimos una rotación de 90 grados.

  self.labels <- getConstructNames(x)[,1]
  noself.labels <- getConstructNames(x)[,2]
  imp.labels <- paste(self.labels,"->",noself.labels,sep = " ")                 # Extraemos los nombres de los constructos

  m <- matrix(nrow = dim, ncol = dim +2)
  diag(m[1:dim,1:dim+1]) = 0
  df <- data.frame(m)
  df[,1] <- self.labels
  df[,dim + 2] <- noself.labels
  colnames(df) <- c("-3", imp.labels, "3")    # Creamos el data frame con los datos de los constructos

  addDataFrame(df, sh,
               row.names = FALSE,
               colStyle = dfs
               )

  addDataFrame(df[1,1:dim +1], sh,
               startColumn = 2,
               row.names = FALSE,
               colnamesStyle = CellStyle(wb) + fill.imp + rotate.90
               )                                                                # Incluimos el data frame con los estilos en el documento

  rows <- getRows(sh,rowIndex = 1)
  setRowHeight(rows,300)
  autoSizeColumn(sh,colIndex = c(1:ncol(df)))                                   # Ajustamos tamaño de las celdas

  name <- paste(name,".xlsx")                                                   # Damos nombre al archivo
  saveWorkbook(wb, file = name)                                                 # Exportamos el archivo


}
################################################################################
