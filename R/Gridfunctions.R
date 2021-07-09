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

# PLANTILLA DE IMPGRID
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
#' @export

templateIMP <- function(x){
  requireNamespace("xlsx")
  dim <- dim(x)[2]

  wb <- createWorkbook()
  sh <- createSheet(wb)

  fill.izq <- CellStyle(wb, fill = Fill("#ffff6d","#ffff6d"))
  fill.der <- CellStyle(wb, fill = Fill("#729fcf","#729fcf"))
  fill.imp <- Fill("#77bc65","#77bc65")
  fill.pun <- CellStyle(wb, fill = Fill("#800080","#800080"))
  lc <- as.character(dim + 2)
  dfs <- list(fill.izq,fill.der)
  names(dfs) <- c(1,dim +2)

  rotate.90 <- Alignment(rotation = 90)

  self.labels <- getConstructNames(x)[,1]
  noself.labels <- getConstructNames(x)[,2]
  imp.labels <- paste(self.labels,"->",noself.labels,sep = " ")

  m <- matrix(nrow = dim, ncol = dim +2)
  diag(m[1:dim,1:dim+1]) = 0
  df <- data.frame(m)
  df[,1] <- self.labels
  df[,dim + 2] <- noself.labels
  colnames(df) <- c("Polo asociado al Yo", imp.labels, "Polo opuesto al Yo")

  addDataFrame(df, sh,
               row.names = FALSE,
               colStyle = dfs
               )

  addDataFrame(df[1,1:dim +1], sh,
               startColumn = 2,
               row.names = FALSE,
               colnamesStyle = CellStyle(wb) + fill.imp + rotate.90
  )
  rows <- getRows(sh,rowIndex = 1)
  setRowHeight(rows,300)
  autoSizeColumn(sh,colIndex = c(1:ncol(df)))
  saveWorkbook(wb, file = "ImpGrid_Template.xlsx")


}
