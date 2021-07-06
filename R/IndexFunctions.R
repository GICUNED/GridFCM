################################################################################
#------------------------------INDEX FUNCTIONS---------------------------------#
################################################################################

# CENTRALIDAD POR GRADO
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

CentDegree <- function(imp, method="simple"){

  result <- list()                                                              # Creamos una lista vacía para luego poder referenciarla.

  N <- dim(imp)[1]

  if(method == "simple" | method == "norm" | method == "ego"){                  # Método simple

    imp.grid <- imp/imp
    imp.grid[is.nan(imp.grid)] <- 0                                             # Convertimos los pesos a 1 para tener en cuenta las relaciones sin ponderar ni dirección.

    Cout <- rowSums(imp.grid)
    Cin <- colSums(imp.grid)                                                    # Sumamos por filas y por columnas para obtener el número de entradas y el número de salidas.
  }

  if(method == "weight" | method == "wnorm"){                                   # Método Ponderado

    imp.grid <- imp/3                                                           #Transformamos la matriz de implicaciones a una matriz de pesos.

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

  result$Entrada <- Cout
  result$Salida <- Cin                                                          # Introducimos los resultados en la lista

  return(result)
}
################################################################################

# MATRIZ DE DISTANCIAS
################################################################################

#' Cálculo de distancia entre constructos en el digrafo (DistMatrix)
#'
#' @description Función que permite calcular la distancia mas corta entre dos
#' constructos en el digrafo.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @return Devuelve la matriz de distancia del digrafo. Matriz que contiene las
#' distancias de los caminos más cortos de un constructo a otro.
#'
#' @examples
#'
#' @export
#'

DistMatrix <- function(imp){

  w.mat <- WeightMatrix(imp)
  w.mat <- as.matrix(w.mat)

  G <- igraph::graph.adjacency(w.mat,mode = "directed",weighted = T)

  result <- igraph::shortest.paths(G, weights = NA)

  return(result)
}
################################################################################

# CENTRALIDAD POR CERCANÍA
################################################################################

#' Cálculo de la centralidad a través de la cercanía (CentClose)
#'
#' @description Función que permite calcular la cercanía de un constructo
#' del resto de constructos del mapa cognitivo borroso.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @return Devuelve un vector con el índice de cercanía de cada uno de los
#' constructos.
#'
#' @examples
#'
#' @export
#'

CentClose <- function(imp, norm = TRUE){

  dist <- DistMatrix(imp)
  N <- dim(dist)[1]
  if(norm){
    result <- (N-1)/(colSums(dist))
  }else{
    result <- 1/(colSums(dist))
  }
  return(result)
}