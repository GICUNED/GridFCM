################################################################################
#------------------------------INDEX FUNCTIONS---------------------------------#
################################################################################

# FUZZY COGNITIVE MAP DENSITY -- density_index()
################################################################################

#' Fuzzy Cognitive Map density -- density_index()
#'
#' @description Función que permite calcular la densidad de aristas dentro del
#' grafo que genera la matriz de implicaciones.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#'
#' @return Devuelve una valor de 0 a 1 que representa la proporcion de aristas
#' existentes en el grafo sobre el número máximo de aristas posibles.
#'
#' @import OpenRepGrid
#'
#' @export

density_index <- function(imp){

  imp.a<- .adaptrepgrid(imp, t = FALSE)                                         # adaptamos el objeto repgrid a una matriz

  n <- ncol(imp.a)

  result <- sum(degree_index(imp)$Outputs)/(n*(n-1))                            # dividimos el número de aristas que tiene el mapa entre el número potencial de aristas

  return(result)
}
################################################################################


# DEGREE INDEX CENTRALITY -- degree_index()
################################################################################

#' Degree Index -- degree_index()
#'
#' @description Función que permite calcular la centralidad de los constructos
#' teniendo en cuenta su grado. Entendiendo este como el grado de conexión que
#' mantiene con el resto de constructos.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param method Método para calcular el grado de centralidad.
#'
#' @return Devuelve una lista con los datos de centralidad por constructo y
#' separados por grado de entrada y grado de salida.
#'
#' @import OpenRepGrid
#'
#' @export

degree_index <- function(imp, method="simple"){


  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                             # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  result <- list()                                                              # Creamos una lista vacía para luego poder referenciarla.
  imp_a <- .adaptrepgrid(imp, t = FALSE)
  N <- dim(imp_a)[1]

  if(method == "simple" | method == "norm" | method == "ego"){                  # Método simple------------------------------------------

    imp.grid <- imp_a/imp_a
    imp.grid[is.nan(imp.grid)] <- 0                                               # Convertimos los pesos a 1 para tener en cuenta las relaciones sin ponderar ni dirección.

    Cout <- rowSums(imp.grid)
    Cin <- colSums(imp.grid)                                                      # Sumamos por filas y por columnas para obtener el número de entradas y el número de salidas.
  }

  if(method == "weight" | method == "wnorm"){                                   # Método Ponderado----------------------------------------

    imp.grid <- .weightmatrix(imp_a)                                              # Transformamos la matriz de implicaciones a una matriz de pesos.

    Cout <- rowSums(abs(imp.grid))
    Cin <- colSums(abs(imp.grid))                                                 # Sumamos los valores absolutos de las ponderaciones
  }

  if(method == "norm" | method == "wnorm"){                                     # Método Normalizado--------------------------------------

    Cout <- Cout/(N-1)
    Cin <- Cin/(N-1)                                                              # Dividimos los vectores de entrada y salida entre N - 1.
  }

  if(method == "ego"){                                                          # Método de densidad de ego-------------------------------------

    Cout <- Cout/(N*(N-1))
    Cin <- Cin/(N*(N-1))                                                          # Dividimos los vectores de entrada y salida entre el número de aristas potenciales.
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

#' Distance Matrix -- dismatrix()
#'
#' @description Función que permite calcular la distancia más corta entre cada
#' uno de los pares de constructos en el digrafo.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param mode Modo de calcular las distancias en función de la dirección de las
#' aristas. Con "out" las calculamos respetando la dirección de las aristas,"in"
#' a través de la inversa de la dirección de las aristas y "all" sin tener en
#' cuenta la dirección.
#'
#' @return Devuelve la matriz de distancia del digrafo. Matriz que contiene las
#' distancias de los caminos más cortos de un constructo a otro.
#'
#' @export
#'

dismatrix <- function(imp,mode="out"){
  imp_a <- .adaptrepgrid(imp, t = FALSE)

  w.mat <- .weightmatrix(imp_a)                                                 # Transformamos la matriz de implicaciones a una matriz de pesos
  w.mat <- as.matrix(w.mat)

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)            # Utilizamos el paquete igraph para calcular las distancias

  result <- igraph::shortest.paths(G, weights = NA,mode = mode)

  return(result)
}
################################################################################

# CLOSENESS CENTRALITY INDEX -- close_index()
################################################################################

#' Closeness index -- close_index()
#'
#' @description Función que permite calcular la cercanía de un constructo
#' del resto de constructos del mapa cognitivo borroso.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param norm Si es TRUE devuelve la cercanía de los constructos normalizada en
#' función del número de vértices. Por defecto se establece en TRUE.
#'
#' @return Devuelve un vector con el índice de cercanía de cada uno de los
#' constructos.
#'
#' @export
#'

close_index <- function(imp, norm = TRUE){


  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                             # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  dist <- dismatrix(imp)                                                        # Calculamos la matriz de distancias
  N <- dim(dist)[1]

  if(norm){
    result <- (N-1)/(colSums(dist))                                             # Se suman las distancias de cada constructo con el resto, y se normalizan si procede.
  }else{
    result <- 1/(colSums(dist))
  }

  names(result) <- poles                                                        # Se nombran los elementos del vector

  return(result)
}
################################################################################

# BETWEENESS CENTRALITY INDEX -- betw_index()
################################################################################

#' Betweeness index -- betw_index()
#'
#' @description Función que permite calcular la intermediación de todos los
#' constructos. Esto es el número de veces que un camino geodésico entre otros
#' dos constructos pasa por dicho constructo en el digrafo.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param norm Si es TRUE devuelve la intermediación de los constructos
#' normalizada en función del número de vértices. Por defecto se establece en
#' TRUE.
#'
#' @return Devuelve un vector con el índice de intermediación de cada uno de los
#' constructos.
#'
#' @export
#'

betw_index <- function(imp,norm=TRUE){

  lpoles <- OpenRepGrid::getConstructNames(imp)[,1]                             # Extraemos los nombres de los constructos.
  rpoles <- OpenRepGrid::getConstructNames(imp)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  imp_a <- imp
  imp_a <- .adaptrepgrid(imp, t = FALSE)

  w.mat <- .weightmatrix(abs(imp_a))
  w.mat <- as.matrix(w.mat)                                                     # Transformamos la matriz de implicaciones a pesos.

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)

  result <- igraph::betweenness(G,normalized = norm,weights = NA )              # Utilizamos el paquete igraph para el calculo de intermediación.

  names(result) <- poles

  return(result)
}
################################################################################

#  PCSD AUC INDEX -- auc_index()
################################################################################

#' PCSD AUC Index -- auc_index()
#'
#' @description Esta función nos devuelve el area debajo de la curva del PCSD
#' para cada uno de los constructos personales.
#'
#' @param grid Repgrid del sujeto. Debe de ser un objeto importando con la
#' función \code{\link{importgrid}}.
#'
#' @param  imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra. Por defecto se estable el último
#' elemento de la Repgrid.
#'
#' @param ... Esta función hereda todos los parámetos de la función
#' \code{\link{pcsd}}
#'
#' @report Devuelve un vector con el area bajo la curva para cada uno de los
#' constructos personales.
#'
#' @import MESS
#'
#' @export

auc_index <- function(grid, imp, ideal=dim(grid)[2],...){

  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(grid,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                        nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Extraemos la matriz de iteraciones hasta su convergencia.
  res <- abs(res - ideal.matrix) / 2                                            # Transformamos la matriz de iteraciones en distancias estandarizadas desde el Yo-Ideal.

  matrix <- matrix(ncol= length(poles), nrow = 1)

  for (n in 1:length(poles)) {                                                  # Calculamos el area bajo la curva para cada constructo
    matrix[,n] <- MESS::auc(c(1:iter), res[,n], type = "spline")/iter
  }

  result <- as.vector(matrix)

  names(result) <- poles


  return(result)
}
################################################################################

#  PCSD STABILITY INDEX -- stability_index()
################################################################################

#' PCSD Stability Index -- stability_index()
#'
#' @description Esta función nos devuelve la desviación típica para cada uno de
#' los constructos personales a lo largo de la iteraciones matemáticas del PCSD.
#'
#' @param grid Repgrid del sujeto. Debe de ser un objeto importando con la
#' función \code{\link{importgrid}}.
#'
#' @param  imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra. Por defecto se estable el último
#' elemento de la Repgrid.
#'
#' @param ... Esta función hereda todos los parámetos de la función
#' \code{\link{pcsd}}
#'
#' @return Devuelve un vector con los valores de la desviación típica para cada
#' uno de los constructos.
#'
#' @import stats
#'
#' @export

stability_index <- function(grid, imp, ideal=dim(grid)[2],...){


  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extraemos los nombres de los constructos.
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(grid,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas.
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Extraemos la matriz de iteraciones hasta su convergencia.
  res <- abs(res - ideal.matrix) / 2                                            # Transformamos la matriz de iteraciones en distancias estandarizadas desde el Yo-Ideal.


  result <- apply(res, 2, sd)                                                   # Calculamos la desviación típica para cada constructro.

  names(result) <- poles                                                        # Damos nombres a los elementos del vector.

  return(result)
  }
################################################################################

#  PCSD SUMMARY -- pcsd_summary()
################################################################################

#' PCSD summary -- pcsd_summary()
#'
#' @description Esta función nos devuelve un resumen sobre el PCSD. Nos informa
#' sobre el valor inicial y final de cada constructo y sobre la diferencia entre
#' ambos.
#'
#' @param grid Repgrid del sujeto. Debe de ser un objeto importando con la
#' función \code{\link{importgrid}}.
#'
#' @param  imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra. Por defecto se estable el último
#' elemento de la Repgrid.
#'
#' @param ... Esta función hereda todos los parámetos de la función
#' \code{\link{pcsd}}
#'
#' @return Devuelve una matriz con el resumen del PCSD.
#'
#'
#' @export

pcsd_summary <- function(grid, imp, ideal=dim(grid)[2],...){



  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  iter <- fcminfer(grid,imp,iter=60,...)$convergence                            # Extraemos la convergencia de la inferencia

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Extraemos la matriz de iteraciones hasta su convergencia.
  res <- abs(res - ideal.matrix) / 2                                            # Transformamos la matriz de iteraciones en distancias estandarizadas desde el Yo-Ideal.



  result <- res[c(1,iter),]                                                     # Extraemos el primer vector y el último vector de la matriz de iteraciones
  result <- t(result)
  result <- cbind(result, result[,2] - result[,1])                              # Calculamos la diferencia entre el primer vector y el último y lo agregamos a los resultados.

  rownames(result) <- poles
  colnames(result) <- c("Initial value", "Final value", "Difference")

  return(result)
}

# PERSONAL CONSTRUCTS SYSTEM DERIVATIVE -- pcsd_derivative()
################################################################################

#' PCSD derivative -- pcsd_derivative()
#'
#' @description Esta función reprensenta la primera derivada para cada una de
#' las curvas del PCSD.
#'
#' @param grid Repgrid del sujeto. Debe de ser un objeto importando con la
#' función \code{\link{importgrid}}.
#'
#' @param  imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra. Por defecto se estable el último
#' elemento de la Repgrid.
#'
#' @param ... Esta función hereda todos los parámetos de la función
#' \code{\link{pcsd}}
#'
#' @return Devuelve una representación gráfica creada con el paquete plotly.
#'
#' @import plotly
#'
#' @export

pcsd_derivative <- function(grid,imp,ideal=dim(grid)[2],...){


  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(grid,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                         nrow = iter, byrow = TRUE)

  res.pre <- fcminfer(grid,imp,iter=iter,...)$values                               # Obtenemos la inferencia del MCB
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

  fig <- plotly::plot_ly(df, x = ~x, y = df[,2], name = poles[1],
                         type = 'scatter', mode = 'lines+markers',
                         line = list(shape = "spline"))

  for (n in 3:(length(poles)+1)) {
    fig <- fig %>% plotly::add_trace(y = df[,n], name = poles[n-1],
                                     mode = 'lines+markers',
                                     line = list(shape = "spline"))
  }

  fig <- fig %>% plotly::layout(xaxis = list(
                                title = "ITERATIONS"),
                                yaxis = list(
                                title = "DERIVATIVE"))

  fig <- fig %>% plotly::layout(legend=list(
                                title=list(text='<b>PERSONAL CONSTRUCTS</b>')))

  fig
}
################################################################################
