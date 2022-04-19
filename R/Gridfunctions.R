################################################################################
##----------------------------#GRID FUNCTIONS##-------------------------------##
################################################################################

# IMPORT IMPGRID -- importimp()
###############################################################################
#' Import ImpGrid -- importimp()
#'
#' @description Función que permite leer una ImpGrid contenida
#' dentro de un archivo xlsx. Para la creación de la plantilla xlsx de la
#' ImpGrid se puede utilizar la función \code{\link{templateimp}}
#'
#' @param path Ruta donde se encuentra el archivo de excel que contiene la
#' ImpGrid.
#'
#' @param ... Esta función hereda todos los parámetros de la función
#' \code{\link{importExcel}} del paquete OpenRepGrid.
#'
#' @return Devuelve un objeto S4 repgrid que contiene los datos de la matriz de
#' implicaciones
#'
#' @export
#'

importimp <- function(path,...){

  result <- importExcel(path, ...)                                              # Importamos la rejilla de implicaciones utilizando el importExcel() del paquete openrepgrid

  return(result)
}

# IMPORT REPGRID -- importgrid()
################################################################################

#' Import RepGrid -- importgrid()
#'
#' @description Función que permite leer una técnica de rejilla contenida
#' dentro de un archivo xlsx.
#'
#' @param path Ruta donde se encuentra el archivo de excel que contiene la
#' la técnica de rejilla.
#'
#' @param ... Esta función hereda todos los parámetros de la función
#' \code{\link{importExcel}} del paquete OpenRepGrid.
#'
#' @return Devuelve un objeto S4 repgrid del paquete OpenRepGrid que contiene la
#' información de la técnica de rejilla.
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

#' Create ImpGrid Template -- templateimp()
#'
#' @description Función que crea en el directorio de trabajo un archivo de excel
#' con una plantilla personalizada de la Rejilla de Implicaciones teniendo como
#' base una RepGrid.
#'
#' @param grid Técnica de Rejilla importada a través de la función
#' \code{\link{importgrid}}. Debe de ser un objeto RepGrid.
#'
#' @param name Nombre para el archivo de salida.
#'
#' @return Exporta un documento xlsx en el directorio de trabajo asignado de la
#' sesión.
#'
#' @export

templateimp <- function(grid,name ="ImpGrid_Template"){

  if(!library(xlsx, quietly = TRUE, logical.return=TRUE)){
    stop(cat("ERROR: You need install xlsx package for this function \n"))
  }

  dim <- dim(grid)[1]                                                           # Guardamos el número de constructos de la rejilla

  wb <- createWorkbook()
  sh <- createSheet(wb)                                                         # Creamos el documento de trabajo y la hoja de trabajo


  fill.izq <- CellStyle(wb, fill = Fill("#ffff6d","#ffff6d"))
  fill.der <- CellStyle(wb, fill = Fill("#729fcf","#729fcf"))
  fill.imp <- Fill("#77bc65","#77bc65")                                         # Establecemos los colores para las celdas


  dfs <- list(fill.izq,fill.der)
  names(dfs) <- c(1,dim +2)                                                     # Establecemos los estilos de las celdas

  rotate.90 <- Alignment(rotation = 90)                                         # Definimos una rotación de 90 grados para el texto.

  self.labels <- getConstructNames(grid)[,1]
  noself.labels <- getConstructNames(grid)[,2]
  imp.labels <- paste(self.labels,"->",noself.labels,sep = " ")                 # Extraemos los nombres de los constructos

  m <- matrix(nrow = dim, ncol = dim +2)
  diag(m[1:dim,1:dim+1]) = 0
  df <- data.frame(m)
  df[,1] <- self.labels
  df[,dim + 2] <- noself.labels
  colnames(df) <- c("-3", imp.labels, "3")                                      # Creamos el data frame con los datos de los constructos

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
