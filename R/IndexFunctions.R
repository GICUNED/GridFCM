################################################################################
#------------------------------INDEX FUNCTIONS---------------------------------#
################################################################################

# CENTRALIDAD POR GRADO
################################################################################

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


