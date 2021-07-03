# IMPORTAR REJILLA DE IMPLICACIONES
###############################################################################

importIMP <- function(path){
  require(readxl)
  xls <- read_excel(path)
  xls <- xls[,-dim(xls)[2]]
  xls <- xls[,-1]
  rownames(xls) <- colnames(xls)
  return(xls)
}

