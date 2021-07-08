################################################################################
##--------------------------#FUNCIONES OCULTAS##------------------------------##
################################################################################

# ADAPTACIÓN DE REPGRID A MATRIZ DE ANALISIS MULTIVARIADO
################################################################################
.adaptrepgrid <- function(x, t=FALSE){

  result <- getRatingLayer(x)

  if(!t){
  result <- t(result)
  }

  return(result)
}
################################################################################

# ALINEAR SEGÚN YO-ACTUAL
################################################################################
.alignbyself <- function(x, self = 1){
  for (i in 1:dim(x)[1]) {
    if(getRatingLayer(x)[i,self] > 4){
      x <- swapPoles(x,i)
    }
  }
  return(x)
}
