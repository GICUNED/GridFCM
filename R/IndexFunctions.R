################################################################################
#------------------------------INDEX FUNCTIONS---------------------------------#
################################################################################

# FUZZY COGNITIVE MAP DENSITY -- density_index()
################################################################################

#' Cálculo de la Densidad del Mapa (IMPdensity)
#'
#' @description Función que permite calcular la densidad de aristas dentro del
#' grafo que genera la matriz de implicaciones.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#'
#' @return Devuelve una valor de 0 a 1 que representa la proporcion de aristas
#' existentes en el grafo sobre el número máximo de aristas posibles.
#'
#' @examples
#'
#' @export
#'

density_index <- function(imp){

  imp_a <- .adaptrepgrid(imp, t = FALSE)
  n <- ncol(imp_a)

  result <- sum(degree_index(imp)$Outputs)/(n*(n-1))

  return(result)
}
################################################################################


# DEGREE INDEX CENTRALITY -- degree_index()
################################################################################

#' Cálculo de la Centralidad a través del Grado (CentDegree)
#'
#' @description Función que permite calcular la centralidad de los constructos
#' teniendo en cuenta su grado. Entendiendo este como el grado de conexión que
#' mantiene con el resto de constructos.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param method Método para calcular el grado de centralidad. Más información
#' escribiendo ?\code{\link{DegreeMethod}} o haciendo click sobre él.
#'
#' @return Devuelve una lista con los datos de centralidad por constructo y
#' separados por grado de entrada y grado de salida.
#'
#' @examples
#'
#' @export
#'

degree_index <- function(imp, method="simple"){


  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  result <- list()                                                              # Creamos una lista vacía para luego poder referenciarla.
  imp_a <- .adaptrepgrid(imp, t = FALSE)
  N <- dim(imp_a)[1]

  if(method == "simple" | method == "norm" | method == "ego"){                  # Método simple

    imp.grid <- imp_a/imp_a
    imp.grid[is.nan(imp.grid)] <- 0                                             # Convertimos los pesos a 1 para tener en cuenta las relaciones sin ponderar ni dirección.

    Cout <- rowSums(imp.grid)
    Cin <- colSums(imp.grid)                                                    # Sumamos por filas y por columnas para obtener el número de entradas y el número de salidas.
  }

  if(method == "weight" | method == "wnorm"){                                   # Método Ponderado

    imp.grid <- imp_a/3                                                           # Transformamos la matriz de implicaciones a una matriz de pesos.

    Cout <- rowSums(abs(imp.grid))
    Cin <- colSums(abs(imp.grid))                                               # Sumamos los valores absolutos de las ponderaciones
  }

  if(method == "norm" | method == "wnorm"){                                     # Método Normalizado

    Cout <- Cout/(N-1)
    Cin <- Cin/(N-1)                                                            # Dividimos los vectores de entrada y salida entre N - 1.
  }

  if(method == "ego"){                                                          # Método de densidad de ego.

    Cout <- Cout/(N*(N-1))
    Cin <- Cin/(N*(N-1))                                                        # Dividimos los vectores de entrada y salida entre el número de aristas potenciales.
  }
  names(Cout) <- poles
  names(Cin) <- poles

  result$Outputs <- Cout
  result$Inputs <- Cin
  result$All <- Cout + Cin                                                      # Introducimos los resultados en la lista

  return(result)
}
################################################################################

# DISTANCE MATRIX -- dismatrix()
################################################################################

#' Cálculo de distancia entre constructos en el digrafo (IMPdistances)
#'
#' @description Función que permite calcular la distancia mas corta entre dos
#' constructos en el digrafo.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param mode Modo de calcular las distancias en función de la dirección de las
#' aristas. "out" las calculamos respetando la dirección de las aristas, "in"
#' a través de la inversa de la dirección de las aristas y "all" sin tener en
#' cuenta la dirección del grafo.
#'
#' @return Devuelve la matriz de distancia del digrafo. Matriz que contiene las
#' distancias de los caminos más cortos de un constructo a otro.
#'
#' @examples
#'
#' @export
#'

dismatrix <- function(imp,mode="out"){
  imp_a <- .adaptrepgrid(imp, t = FALSE)

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.matrix(w.mat)

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)

  result <- igraph::shortest.paths(G, weights = NA,mode = mode)

  return(result)
}
################################################################################

# CLOSENESS CENTRALITY INDEX -- close_index()
################################################################################

#' Cálculo de la centralidad a través de la cercanía (CentClose)
#'
#' @description Función que permite calcular la cercanía de un constructo
#' del resto de constructos del mapa cognitivo borroso.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param norm Si es TRUE devuelve la cercanía de los constructos normalizada en
#' función del número de vértices. Por defecto se establece en TRUE.
#'
#' @return Devuelve un vector con el índice de cercanía de cada uno de los
#' constructos.
#'
#' @examples
#'
#' @export
#'

close_index <- function(imp, norm = TRUE){


  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  dist <- dismatrix(imp)
  N <- dim(dist)[1]
  if(norm){
    result <- (N-1)/(colSums(dist))
  }else{
    result <- 1/(colSums(dist))
  }
  names(result) <- poles
  return(result)
}
################################################################################

# BETWEENESS CENTRALITY INDEX -- betw_index()
################################################################################

#' Cálculo de la centralidad a través de la intermediación (CentBetw)
#'
#' @description Función que permite calcular la intermediación de un
#' constructo. Esto es el número de veces que un camino geodésico entre otros
#' dos constructos pasa por dicho constructo.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param norm Si es TRUE devuelve la intermediación de los constructos
#' normalizada en función del número de vértices. Por defecto se establece en
#' TRUE.
#'
#' @return Devuelve un vector con el índice de intermediación de cada uno de los
#' constructos.
#'
#' @examples
#'
#' @export
#'

betw_index <- function(imp,norm=TRUE){

  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                             # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  imp_a <- imp
  imp_a <- .adaptrepgrid(imp, t = FALSE)

  w.mat <- .weightmatrix(abs(imp_a))
  w.mat <- as.matrix(w.mat)

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)

  result <- igraph::betweenness(G,normalized = norm,weights = NA )
  names(result) <- poles

  return(result)
}
################################################################################

#  PCSD AUC INDEX -- auc_index()
################################################################################

#' @import MESS
#'
#' @export

auc_index <- function(x, imp, ideal=dim(x)[2],...){

  require(MESS)

  lpoles <- OpenRepGrid::getConstructNames(x)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(x)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(x,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(x)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(x,imp,iter=iter,...)$values
  res <- abs(res - ideal.matrix) / 2

  matrix <- matrix(ncol= length(poles), nrow = 1)

  for (n in 1:length(poles)) {
    matrix[,n] <- auc(c(1:iter), res[,n], type = "spline")/iter
  }

  colnames(matrix) <- poles
  rownames(matrix) <- NULL

  return(matrix)
}
################################################################################

#  PCSD STABILITY INDEX -- stability_index()
################################################################################

#' @import MESS
#'
#' @export

stability_index <- function(x, imp, ideal=dim(x)[2],...){

  require(MESS)

  lpoles <- OpenRepGrid::getConstructNames(x)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(x)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(x,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(x)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(x,imp,iter=iter,...)$values
  res <- abs(res - ideal.matrix) / 2


  result <- apply(res, 2, sd)
  names(result) <- poles

  return(result)
  }
################################################################################

#  PCSD SUMMARY -- pcsd_summary()
################################################################################

#' @import MESS
#'
#' @export

pcsd_summary <- function(x, imp, ideal=dim(x)[2],...){

  require(MESS)

  lpoles <- OpenRepGrid::getConstructNames(x)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(x)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(x,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(x)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(x,imp,iter=iter,...)$values
  res <- abs(res - ideal.matrix) / 2


  result <- res[c(1,iter),]
  result <- t(result)
  result <- cbind(result, result[,2] - result[,1])

  rownames(result) <- poles
  colnames(result) <- c("Initial value", "Final value", "Difference")

  return(result)
}
# PERSONAL CONSTRUCTS SYSTEM DYNAMICS PLOT -- pcsd()
################################################################################


#'
#' @import plotly
#'
#' @export

pcsd_derivative <- function(x,imp,ideal=dim(x)[2],...){


  lpoles <- OpenRepGrid::getConstructNames(x)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(x)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(x,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(x)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                         nrow = iter, byrow = TRUE)

  res.pre <- fcminfer(x,imp,iter=iter,...)$values                               # Obtenemos la inferencia del MCB
  res.pre <- abs(res.pre - ideal.matrix) / 2


  x <- c(0:(iter-2))
  y <- c(0:length(poles))

  res <- matrix(ncol = length(poles), nrow = iter - 1)

  for (i in 1:length(poles)) {
    res[,i] <- diff(res.pre[,i])/diff(0:(iter-1))
  }

  y <- as.character(y)

  df <- data.frame(x,res)                                                       # Confeccionamos un dataframe con las distancias estandarizadas entre los resultados y el ideal
  colnames(df) <- y

  fig <- plot_ly(df, x = ~x, y = df[,2], name = poles[1], type = 'scatter',
                 mode = 'lines+markers',line = list(shape = "spline"))
  for (n in 3:(length(poles)+1)) {
    fig <- fig %>% add_trace(y = df[,n], name = poles[n-1], mode = 'lines+markers'
                             ,line = list(shape = "spline"))
  }
  fig <- fig %>% layout(title="PCSD DERIVATIVE",
                        xaxis = list(
                          title = "ITERATIONS"),
                        yaxis = list(
                          title = "DERIVATIVE"
                          )
  )
  fig <- fig %>% layout(legend=list(
    title=list(text='<b>PERSONAL CONSTRUCTS</b>')))

  fig
}
################################################################################
