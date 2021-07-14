#' @title GRID FCM
#'
#' @description Paquete de R para la creación de mapas cognitivos borrosos a través de la técnica de rejilla
#'
#' @author Alejandro Sanfeliciano (GICUNED)
#'
#' @section Functions:
#'
#' \strong{Funciones de Utilidades:}
#'
#' \code{\link{importGRID}}
#'
#' \code{\link{importIMP}}
#'
#' \code{\link{templateIMP}}
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
#' \code{\link{IMPdensity}}
#'
#' \code{\link{IMPdistances}}
#'
#' \code{\link{CentDegree}}
#'
#' \code{\link{CentClose}}
#'
#' \code{\link{CentBetw}}
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
#' @description
#' El paquete GridFCM permite cuatro funciones umbral, dos de ellas discretas y otras dos continuas:
#'
#' \strong{Funciones discretas}
#'
#' \emph{\strong{Función bivalente:}} La función bivalente devuelve un valor 1 al vértice cuando la activación que le llega es superior a 0,y devuelve 0 cuando
#' esa activacion es menor o igual que 0. Permite que los vértices tomen valores discretos y dicotómicos (activados o desactivados). Para utilizar esta función debemos escribir
#' como argumento `thr = "b"` en la función \code{\link{FuzzyInfer}}.
#'
#' \emph{\strong{Función trivalente:}} La función trivalente devuelve un valor 1 al vértice cuando la activación que le llega es superior a 0, devuelve 0 cuando
#' esa activacion es igual a 0, y devuelve -1 si esa activación es inferior a 0. Permite que los vértices tomen valores discretos, y a diferencia de la bivalente, devuelve
#' tres posibles valores distintos. Para utilizar esta función debemos escribir como argumento `thr = "tri"` en la función \code{\link{FuzzyInfer}}.
#'
#' \strong{Funciones continuas}
#'
#' \emph{\strong{Función sigmoidal:}} La función sigmoidal es la versión continua de la función bivalente. Permite establecer un rango de activación del vértice entre 0 y 1
#' . Si utilizamos esta función debemos específicar un lambda asociado que define el poder de discriminación de la función; valores altos de lambda acercan la función sigmoidal
#' a la función bivalente, valores bajos la acercan a una función lineal. Para utilizar esta función debemos escribir como argumento thr = "s" en la función \code{\link{FuzzyInfer}}
#'
#' \emph{\strong{Función tangente hiperbólica:}} La función tangente hiperbólica es la versión continua de la función trivalente. Permite establecer un rango de activación del vértice entre -1 y 1
#' . Si utilizamos esta función debemos específicar un lambda asociado que define el poder de discriminación de la función; valores altos de lambda acercan la función tangente hiperbólica
#' a la función trivalente, valores bajos la acercan a una función lineal. Para utilizar esta función debemos escribir como argumento thr = "t" en la función \code{\link{FuzzyInfer}}
#'
NULL

#' @title Tipos de layouts para el digrafo
#' @name GraphLayouts
#'
NULL

#' @title Métodos para el cálculo de centralidad a través del grado
#' @name DegreeMethod
#'
NULL
