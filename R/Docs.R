#' @title GRID FCM
#'
#' @description R package for applying Fuzzy Cognitive Maps to Personal
#' Construct Psychology.
#'
#' \strong{Logistic Functions:}
#'
#' \code{\link{importgrid}}
#'
#' \code{\link{importimp}}
#'
#' \code{\link{templateimp}}
#'
#' \strong{Calc Functions:}
#'
#' \code{\link{actvector}}
#'
#' \code{\link{dismatrix}}
#'
#' \code{\link{fcminfer}}
#'
#' \code{\link{pcsd_summary}}
#'
#' \strong{Plot Functions:}
#'
#' \code{\link{fcmdigraph}}
#'
#' \code{\link{fcmdigraph3D}}
#'
#' \code{\link{idealdigraph}}
#'
#' \code{\link{pcsd}}
#'
#' \code{\link{pcsd_derivative}}
#'
#' \strong{Index Functions:}
#'
#' \code{\link{density_index}}
#'
#' \code{\link{degree_index}}
#'
#' \code{\link{close_index}}
#'
#' \code{\link{betw_index}}
#'
#' \code{\link{auc_index}}
#'
#' \code{\link{stability_index}}
#'
#' @author Alejandro Sanfeliciano -- sanfeliciano.alejandro@gmail.com
#'
#' @docType package
#'
"_PACKAGE"

#' @title Tipos de funciones de propagación
#'
#' @name propfunctions
#'
#' @description
#' El paquete GridFCM permite tres funciones de propagación:
#'
#' \strong{Inferencia de Kosko:} Consiste en un producto matricial entre el vector de activación y la matriz de pesos. La nueva activación de los vértices depende de la activación de
#' sus transmisores y del peso de sus relaciones. Para utilizarla en la función \code{\link{fcminfer}} el argumento infer debe de ser igual a "k".
#'
#' \strong{Inferencia Modificada de Kosko:} Consiste en sumarle al estado actual de los vértices el producto matricial entre el vector de activación y la matriz de pesos. La nueva
#' activación de los vértices depende de la activación de sus transmisores, del peso de sus relaciones y del estado previo de la red. Para utilizarla en la función.
#' \code{\link{fcminfer}} el argumento infer debe de ser igual a "mk".
#'
#' \strong{Inferencia reescalada:} Similar a la modificada de Kosko pero con un reescalado de los valores. Para utilizarla en la función \code{\link{fcminfer}} el argumento infer
#'  debe de ser igual a "r".
#'
NULL

#' @title Tipos de funciones umbral
#' @name thrfunctions
#' @description
#' El paquete GridFCM permite cuatro funciones umbral, dos de ellas discretas y otras dos continuas:
#'
#' \strong{Funciones discretas}
#'
#' \emph{\strong{Función bivalente:}} La función bivalente devuelve un valor 1 al vértice cuando la activación que le llega es superior a 0,y devuelve 0 cuando
#' esa activacion es menor o igual que 0. Permite que los vértices tomen valores discretos y dicotómicos (activados o desactivados). Para utilizar esta función debemos escribir
#' como argumento `thr = "b"` en la función \code{\link{fcminfer}}.
#'
#' \emph{\strong{Función trivalente:}} La función trivalente devuelve un valor 1 al vértice cuando la activación que le llega es superior a 0, devuelve 0 cuando
#' esa activacion es igual a 0, y devuelve -1 si esa activación es inferior a 0. Permite que los vértices tomen valores discretos, y a diferencia de la bivalente, devuelve
#' tres posibles valores distintos. Para utilizar esta función debemos escribir como argumento `thr = "tri"` en la función \code{\link{fcminfer}}.
#'
#' \strong{Funciones continuas}
#'
#' \emph{\strong{Función sigmoidal:}} La función sigmoidal es la versión continua de la función bivalente. Permite establecer un rango de activación del vértice entre 0 y 1
#' . Si utilizamos esta función debemos específicar un lambda asociado que define el poder de discriminación de la función; valores altos de lambda acercan la función sigmoidal
#' a la función bivalente, valores bajos la acercan a una función lineal. Para utilizar esta función debemos escribir como argumento thr = "s" en la función \code{\link{fcminfer}}
#'
#' \emph{\strong{Función tangente hiperbólica:}} La función tangente hiperbólica es la versión continua de la función trivalente. Permite establecer un rango de activación del vértice entre -1 y 1
#' . Si utilizamos esta función debemos específicar un lambda asociado que define el poder de discriminación de la función; valores altos de lambda acercan la función tangente hiperbólica
#' a la función trivalente, valores bajos la acercan a una función lineal. Para utilizar esta función debemos escribir como argumento thr = "t" en la función \code{\link{fcminfer}}
#'
NULL

#' @title Tipos de layouts para el digrafo
#' @name digraphlayouts
#' @description
#' El paquete GridFCM permite cinco layouts para el digrafo:
#'
#' \strong{Graphopt:} Este layout representa el digrafo creando clusters en función de la influencia que tienen los constructos entre si. Cuanto más cerca estén dos constructos,
#' mayor influencia hay entre ellos. Para utilizar este layout debemos igualar el atributo layout a "graphopt".
#'
#' \strong{Multidimensional Scaling:} Este layout establece las coordenadas de los vértices a través de una escalado multidimensional de la matriz de implicaciones.
#' Cuanto más cerca estén dos constructos, una mayor similitud hay entre ellos en referencia a sus implicaciones. Para utilizar este layout debemos igualar el atributo layout a "mds".
#'
#' \strong{Reingold-tilford:} Este layout establece las coordenadas de los vértices siguiendo el algoritmo propuesto por Reingold y Tilford. Para utilizar este layout, podemos igualar
#' el atributo layout a "rtcircle" si lo queremos en formato circular, o a "tree" si lo queremos en formato de arbol.
#'
#' \strong{Circle:} Este layout coloca los vértices en círculo de forma arbitraria sin ningún significado adicional. Es util para representar una imagen limpia y evitar superposiciones.
#' Para utilizar este layout debemos igualar el atributo layout a "circle".
#'
#' \strong{Grid:} Este layout coloca los vértices siguiendo una rejilla de forma arbitraria sin ningún significado adicional. Es util para representar una imagen limpia y evitar
#' superposiciones. Para utilizar este layout debemos igualar el atributo layout a "grid".
NULL
