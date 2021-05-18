################################################################################
##---------------#FUNCIONES DE MAPAS COGNITIVOS BORROSOS##--------------------##
################################################################################

################################################################################
SelectConstructs <- function(x){

   #Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(class(x)[1] == "repgrid"){
    x <- .adaptrepgrid(x)
  }

  constructs <- colnames(x)
  constructs <- as.matrix(constructs, ncol = 1)
  cat("############################################################## \n")
  cat("SELECCIONA LOS CONSTRUCTOS QUE DESEAS TENER EN TU NUEVA MATRIZ \n")
  cat("############################################################## \n")
  print(constructs)
  constructs.selected <- scan(what = "numeric", nmax = dim(x)[2])

  constructs.selected <- as.vector(constructs.selected)
  constructs.selected <- as.numeric(constructs.selected)

    result <- x[constructs.selected,constructs.selected]

    return(result)
}
################################################################################
WeightMatrix <- function(x,imp){
}
