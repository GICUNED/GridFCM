#' @title GRID FCM
#'
#' @description Paquete de R para la creación de mapas cognitivos borrosos a través de la técnica de rejilla
#'
#' @author Alejandro Sanfeliciano (GICUNED)
#'
#' @section Functions:
#'
#' \strong{Funciones Funcionales:}
#'
#' \code{\link{importIMP}}
#'
#' \code{\link{ActVector}}
#'
#' \strong{Funciones de Representación:}
#'
#' \code{\link{FuzzyInfer}}
#'
#' \code{\link{FuzzyMap}}
#'
#' \code{\link{FuzzyMap3D}}
#'
#' \code{\link{IdealMap}}
#'
#' \strong{Funciones de Índices:}
#'
#' \code{\link{CentDegree}}
#'
#' \code{\link{CentClose}}
#'
#' \code{\link{DistMatrix}}
#'
#' @docType package
#'
"_PACKAGE"

#' @title Tipos de funciones de propagación
#' @name PropFunctions
#' @description
#' El paquete GridFCM permite tres funciones de propagación:
#'
#' \strong{Inferencia de Kosko:} Consiste en un producto matricial entre el vector de activación y la matriz de pesos. La nueva activación de los vértices depende de la activación de
#' sus transmisores y del peso de sus relaciones. Para utilizarla en la función \code{\link{FuzzyInfer}} el argumento infer debe de ser igual a "k".
#'
#' \strong{Inferencia Modificada de Kosko:} Consiste en sumarle al estado actual de los vértices el producto matricial entre el vector de activación y la matriz de pesos. La nueva
#' activación de los vértices depende de la activación de sus transmisores, del peso de sus relaciones y del estado previo de la red. Para utilizarla en la función
#' \code{\link{FuzzyInfer}} el argumento infer debe de ser igual a "mk".
#'
#' \strong{Inferencia reescalada:} Similar a la modificada de Kosko pero con un reescalado de los valores. Para utilizarla en la función \code{\link{FuzzyInfer}} el argumento infer
#'  debe de ser igual a "r".
#'
NULL

#' @title Tipos de funciones umbral
#' @name ThrFunctions
#'
NULL

#' @title Tipos de layouts para el digrafo
#' @name GraphLayouts
#'
NULL

#' @title Métodos para el cálculo de dentralidad a través del grado
#' @name DegreeMethod
#'
NULL
