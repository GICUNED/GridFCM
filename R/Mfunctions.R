################################################################################
##---------------#FUNCIONES DE ANALISIS MULTIVARIADO##------------------------##
################################################################################




################################################################################
#---------------#Vector de Medias
cmean <- function(x){
  require(OpenRepGrid)

  #El siguiente código adapta los datos leídos de OpenRepGrid a una estructura de matriz.
  if(class(x)[1] == "repgrid"){
    x <- .adaptrepgrid(x)
  }

  #Se calcula la dimensión de los elementos de la matriz.
  dim.matrix <- dim(x)[1]

  #Aplicamos la fórmula del vector de medias
  result <- (1/dim.matrix) * t(x) %*% .uno(x,1)
  colnames(result) <- c("Media")

  return(result)
}
################################################################################
#----------------#Matriz de Datos Centrados

Cmatrix <- function(x){
  require(OpenRepGrid)

  #El siguiente código adapta los datos leídos de OpenRepGrid a una estructura de matriz.
  if(class(x)[1] == "repgrid"){
   x <- .adaptrepgrid(x)
  }

  #Formula para la matriz de datos centrados
  result <- x - .uno(x,1) %*% t(cmean(x))


  return(result)
}

################################################################################
#-----------------#Matriz de Covarianzas

Smatrix <- function(x, corrected = FALSE){
  require(OpenRepGrid)

  #Se calcula la dimensión de los elementos de la matriz.
  dim.matrix <- dim(x)[1]

  #Se aplica la fórmula correspondiente para el cálculo de matriz de covarianzas en función de si se quiere corregida o no.
  if(corrected){
    result <- (1/(dim.matrix-1)) * t(Cmatrix(x)) %*% Cmatrix(x)
  }
  else{
    result <- (1/dim.matrix) * t(Cmatrix(x)) %*% Cmatrix(x)
  }


  return(result)
}

################################################################################
##--------------------#Matriz de Correlaciones
Rmatrix <- function(x){

  #Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(inherits(x,"repgrid")){
    x <- .adaptrepgrid(x)
  }

  #Construimos la matriz D(S)^-1/2 (DS1).
  dim.matrix <- dim(x)[2]
  DS <- matrix(c(rep(0,dim.matrix*dim.matrix)), ncol = dim.matrix)
  diag(DS) <- diag(Smatrix(x))
  DS1 <- solve(sqrt(DS))

  #Aplicamos la formula de la matriz de correlaciones.
  result <- DS1 %*% Smatrix(x) %*% DS1
  rownames(result) <- colnames(x)
  colnames(result) <- colnames(x)

  return(result)
}
################################################################################
##-------------------#Matriz de Precisión
PREmatrix <- function(x){
  if(det(Smatrix(x)) < 1){
    require(pracma)
    result <- pinv(Smatrix(x))
  }
  else{
  result <- solve(Smatrix(x))
  }
  return(result)
}

################################################################################
##--------------------#Matriz de Correlaciones Parciales
Pmatrix <- function(x){

  #Comprobamos que tengamos la librería de OpenRepGrid.
  require(OpenRepGrid)

  #Adaptamos el formato de OpenRepGrid a una matriz estandar.
  if(class(x)[1] == "repgrid"){
    x <- .adaptrepgrid(x)
  }

  #Construimos una matriz - I de dimensiones adecuadas al número de constructos.
  d <- dim(x)[2]
  NUNO <- -diag(d)

  #Construimos la matriz diagonal de la matriz de precisión.
  D <- d * d
  DS <- matrix(rep(0,D), ncol= d)
  diag(DS) <- diag(PREmatrix(x))

  #aplicamos la fórmula de la matriz de de correlaciones parciales.
  result <- NUNO %*% solve(DS)^(1/2) %*% PREmatrix(x) %*% solve(DS)^(1/2)

  #Corregimos la diagonal de la matriz y le damos nombres a filas y columnas.
  diag(result) <- c(rep(1,d))
  rownames(result) <- colnames(x)
  colnames(result) <- colnames(x)
  return(result)
}

##----------------------##FUNCIONES INTERNAS OCULTAS##-----------------------###


################################################################################
#----------------------#Vector de Unos
.uno <- function(x, n){

  dim.matrix <- dim(x)[n]
  result <- c(rep(1,dim.matrix))

  return(result)
}


