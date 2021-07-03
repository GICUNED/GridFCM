################################################################################
##---------------#FUNCIONES DE MAPAS COGNITIVOS BORROSOS##--------------------##
################################################################################


# SELECCIÓN DE SUBCONJUNTOS DE LA MATRIZ
################################################################################
SelectConstructs <- function(x, sel.vec){

  if(class(x)[1] == "repgrid"){
    x <- .adaptrepgrid(x)
  }
  constructs.selected <- sel.vec
  constructs.selected <- as.vector(constructs.selected)
  constructs.selected <- as.numeric(constructs.selected)

    result <- x[constructs.selected,constructs.selected]

    return(result)
}
################################################################################


# VECTOR DE ACTIVACIÓN
################################################################################

#' Crear Vector de Activación (ActVector)
#' @description Función que extrae un vector de un elemento de la rejilla y lo
#' transforma en un vector de activación apropiado para utilizarse a la hora de
#' crear un mapa cognitivo borroso.
#'
#' @param x Rejilla del sujeto desde donde queremos extraer el vector de
#' activación. Debe de ser un objeto de la clase repgrid del paquete
#' OpenRepGrid.
#' @param col.element Elemento desde el que extraemos el vector de activación.
#' Por defecto se establece el primer elemento, que coincide normalmente con el
#' Yo-Actual.
#'
#' @return Devuelve un vector que contiene los pesos para cada uno de los
#' constructos asociados a un elemento.
#'
#' @examples
#'
#' @export


ActVector <- function(x, col.element = 1){
  require(OpenRepGrid)                                                          # Cargamos OpenRepGrid
  vector <- getRatingLayer(x)[,col.element]                                     # Extraemos el vector de la rejilla
  result <- (vector - getScaleMidpoint(x))/((getScale(x)[2]-1)/2)               # Transformamos el vector en  un intervalo [-1,1]
  return(result)
}
################################################################################


# MATRIZ DE PESOS SEGÚN IMPGRID HINKLE
################################################################################

WeightMatrix <- function(x){
  result <- x/3
  return(result)
}
################################################################################

# INFERENCIA DE ESCENARIOS FUTUROS
################################################################################

#' Inferencia de Escenarios Futuros (FuzzyInfer)
#'
#' @description Función que infiere los cambios dentro un mapa cognitivo borroso
#' y lo expresa iteración a iteración. Permite la representación de un Gráfico
#' de Comportamiento vs. Tiempo (GCT).
#'
#' @param x Rejilla del sujeto desde donde queremos realizar las inferencias.
#' Debe de ser un objeto de la clase repgrid del paquete OpenRepGrid.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param act.vec Vector de activación creado a través de
#' \code{\link{ActVector}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra.
#'
#' @param infer Función de Propagación para la inferencia de escenarios.
#'
#' @param thr Función Umbral para la inferencia de escenarios.
#'
#' @param lambda Valor de lambda de la función umbral.
#'
#' @param iter Número de iteraciones que se desea inferir
#'
#' @param graph Devuelve un Gráfico de Comportamiento vs. Tiempo cuando es
#' \code{TRUE}
#'
#' @return Devuelve una lista (list) que contiene por filas cada uno de los
#' vectores de esenario en función del número de iteraciones.
#'
#' @examples
#'
#' @export


FuzzyInfer <- function(x, imp, act.vec, ideal = dim(x)[2], infer= "mk",
                       thr= "t", lambda = 1 , iter = 30, graph = TRUE){
  require(OpenRepGrid)                                                          # Cargamos las librerías necesarias para la función

  w.mat <- WeightMatrix(imp)
  w.mat <- as.data.frame(w.mat)                                                 # Transformamos la matriz de implicaciones en una matriz de pesos

  ideal.vector <- getRatingLayer(x)[,11]
  ideal.vector <- (ideal.vector - 4)/3                                          # Extraemos el vector del ideal y lo ponemos en formato de intervalo [-1,1]


  result <- fcm::fcm.infer(act.vec, weight_mat = w.mat, infer = infer,
                      transform = thr, lambda = lambda, iter = iter)            # Aplicamos la función de fcm.infer del paquete fcm para hacer la inferencia


  if(graph){                                                                    # Comprobamos si el usuario quiere graficar la matriz de escenarios en un GCT.
    require(reshape2)
    require(ggplot2)                                                            # Cargamos las librerías para el gráfico.

    ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),
                           nrow = iter, byrow = TRUE)                           # Construimos una matriz con el ideal repetido por columnas para luego calcular las distancias.
    iterations <- as.numeric(rownames(result$values))
    df <- data.frame(iterations, abs((result$values - ideal.matrix)/2))
    df2 <- melt(df, id="iterations")                                            # Creamos un dataframe que contiene las distancias del ideal agrupadas por constructos
                                                                                # y separadas para cada iteración.
    plot(ggplot(data=df2,aes(x=iterations, y=value, group=variable,
                             color=variable)) + theme_bw() + geom_line(size=0.7)
         + geom_point(size = 3) + labs(x="Iteraciones",
                                       y="Distancia respecto el ideal",
                                       color="Constructos Personales"))         # Utilizamos el GGplot para generar un gráfico del dataframe y generar el GCT.
  }
  return(result)
}
################################################################################

# DIGRAFO DEL MAPA COGNITIVO BORRROSO
################################################################################

#' Digrafo del Mapa Cognitivo Borroso (FuzzyMap)
#'
#' @description Función que nos dibuja un digrafo del Mapa Cognitivo Borroso del
#' sistema de constructos de un individuo a través de una técnica de rejilla y
#' una rejilla de implicaciones.
#'
#' @param x Rejilla del sujeto del que queremos dibujar el Mapa Cognitivo
#' Borroso. Debe de ser un objeto de la clase repgrid del paquete OpenRepGrid.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param results inferencia de escenarios creada con \code{\link{FuzzyInfer}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra.
#'
#' @param niter Selección del vector de escenario que se quiere representar en
#' el mapa. Expresado a través del número de iteraciones.
#'
#' @param layout Layout que se quiere utilizar para representar el mapa.
#'
#' @param edge.width Escalar del grosor de las aristas.
#'
#' @param vertex.size Escalar del tamaño de los vértices.
#'
#' @param legend Dibuja una leyenda con TRUE.
#'
#' @return Devuelve una representación gráfica de un digrafo de un Mapa
#' Cognitivo Borroso.
#'
#' @examples
#'
#' @export


FuzzyMap <- function(x, imp, results, ideal = dim(x)[2], niter = 30,
                     layout = "rtcircle", edge.width = 1.5, vertex.size = 1,
                     legend = FALSE ){
  require(igraph)                                                               # Cargamos las librerías necesarias.

  lpoles <- getConstructNames(x)[,1]
  rpoles <- getConstructNames(x)[,2]                                            # Extraemos los nombres de los polos de los contrusctos de la Rejilla.

  w.mat <- WeightMatrix(imp)
  w.mat <- as.matrix(w.mat)                                                     # Transformamos las implicaciones en pesos.

  results <- as.numeric(as.data.frame(results)[niter,])                         # Extraemos el vector de escenario seleccionado por el usuario.


  n <- 1
  for (integer in results) {
    if(integer != 0){
    integer.value <- integer / abs(integer)
    w.mat[,n] <- w.mat[,n] * integer.value
    w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }                                                                             # Orientamos la matriz de pesos en funcíon del estado actual de la matriz.
                                                                                # Esto sirve para cambiar el color de las aristas en función del estado de los vértices


  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Creamos un gráfico primitivo y poco a poco lo vamos configurando.

  E(graph.map)$color <- "black"
  n <- 1
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
    n <- n + 1
  }                                                                             # Damos color a las aristas en función del tipo de relación.

  edge.curved <- rep(0, length(E(graph.map)))
  n <- 1
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[M,N] != 0 && w.mat[N,M] != 0){
        edge.curved[n] <- 0.25
      }
      if(w.mat[N,M] != 0){
        n <- n + 1

      }
    }
  }                                                                             # Damos curvatura a las aristas para evitar superposiciones en los casos de bicausalidad.

  n <- 1
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[N,M] != 0){
        E(graph.map)$width[n] <- abs(edge.width * w.mat[N,M])
        n <- n + 1
      }
    }
  }                                                                             # Damos grosor a las aristas en función de los pesos.

  V(graph.map)$color <- "black"
  n <- 1
  for (pole.vertex in results) {
  if(getRatingLayer(x)[,ideal][n] > 4){
    if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
    else{
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
      else{
        if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
      }
    }
  }
    if(getRatingLayer(x)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(x)[,ideal][n] == 4){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }                                                                             # Damos color a los vértices en función de su orientación al ideal.

  n <- 1
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"-",
                                                                rpoles[n])}
      }
    }
    n <- n + 1
  }                                                                             # Damos nombre a los vértices en función del polo que se encuentra activado.

  V(graph.map)$size <- 1
  n <- 1
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
    n <- n + 1
  }                                                                             # Damos tamaño a los vértices en función de su grado de activación.

  E(graph.map)$arrow.size <- edge.width * 0.3
  V(graph.map)$shape <- "circle"
  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"                                         # Realizamos un serie de retoques finales para mejorar la visualización.


  if(layout == "rtcircle"){
    graph.map <- add_layout_(graph.map,as_tree(circular = TRUE))
  }
  if(layout == "tree"){
    graph.map <- add_layout_(graph.map,as_tree())
  }
  if(layout == "circle"){
    graph.map <- add_layout_(graph.map,in_circle())
  }
  if(layout == "graphopt"){
    graph.map <- add_layout_(graph.map,with_graphopt())
  }
  if(layout == "mds"){
    graph.map <- add_layout_(graph.map,with_mds())
  }
  if(layout == "grid"){
    graph.map <- add_layout_(graph.map,on_grid())
  }                                                                             # Diferentes layouts para los mapas cognitivos borrosos.

  plot.igraph(graph.map, edge.curved = edge.curved)                             # Ejecutamos el gráfico.

  if(legend){
    poles <- paste(lpoles,rpoles,sep = " - ")
    poles <- paste(c(1:length(poles)),poles,sep = ". ")
    legend("topright",legend = poles, cex = 0.7,
           title = "Constructos Personales")
  }                                                                             # Dibujamos la leyenda del mapa cognitivo borroso.
}

################################################################################

# DIGRAFO DEL MAPA COGNITIVO BORROSO EN 3D
################################################################################

#' Digrafo del Mapa Cognitivo Borroso en 3D (FuzzyMap3D)
#'
#' @description Función que nos dibuja un digrafo del Mapa Cognitivo Borroso en
#' tres dimensiones del sistema de constructos de un individuo a través de una
#' técnica de rejilla y una rejilla de implicaciones. Utilizando un layout de
#' escalado multidimensional.
#'
#' @param x Rejilla del sujeto del que queremos dibujar el Mapa Cognitivo
#' Borroso. Debe de ser un objeto de la clase repgrid del paquete OpenRepGrid.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param results inferencia de escenarios creada con \code{\link{FuzzyInfer}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra.
#'
#' @param niter Selección del vector de escenario que se quiere representar en
#' el mapa. Expresado a través del número de iteraciones.
#'
#' @param edge.width Escalar del grosor de las aristas.
#'
#' @param vertex.size Escalar del tamaño de los vértices.
#'
#' @return Devuelve una representación gráfica de un digrafo de un Mapa
#' Cognitivo Borroso en un escalado multidimensional de 3 dimensiones.
#'
#' @examples
#'
#' @export
#'
FuzzyMap3D <- function(x, imp, results, ideal = dim(x)[2],niter=30,
                       edge.width=2, vertex.size =1){
  require(igraph)                                                               # Cargamos la librería igraph para dibujar y configurar el digrafo.

  lpoles <- getConstructNames(x)[,1]
  rpoles <- getConstructNames(x)[,2]                                            # Extraemos los nombres de los polos de los constructos.

  w.mat <- WeightMatrix(imp)
  w.mat <- as.matrix(w.mat)                                                     # Transformamos la matriz de implicaciones en una matriz de pesos

  results <- as.numeric(as.data.frame(results)[niter,])                         # Extraemos el vector de escenario que quiere representar el usuario

  n <- 1
  for (integer in results) {
    if(integer != 0){
      integer.value <- integer / abs(integer)
      w.mat[,n] <- w.mat[,n] * integer.value
      w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }                                                                             # Orientamos la matriz de pesos en funcíon del estado actual de la matriz.
                                                                                # Esto sirve para cambiar el color de las aristas en función del estado de los vértices.

  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Creamos el grafo primitivo y lo vamos configurando poco a poco.

  V(graph.map)$size <- 1
  n <- 1
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
    n <- n + 1
  }                                                                             # Damos tamaño a los vértices en función de su grado de activación

  n <- 1
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"-",
                                                                rpoles[n])}
      }
    }
    n <- n + 1
  }                                                                             # Damos nombre a los vértices según que polo se encuentre activado

  V(graph.map)$color <- "black"
  n <- 1
  for (pole.vertex in results) {
    if(getRatingLayer(x)[,ideal][n] > 4){
      if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(x)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(x)[,ideal][n] == 4){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }                                                                             # Damos color a los vértices según su orientación al ideal.

  E(graph.map)$color <- "black"
  n <- 1
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
    n <- n + 1
  }                                                                             # Damos color a las aristas en función del tipo de causalidad.

  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"
  V(graph.map)$label.dist <- 1.5                                                # Hacemos unos últimos retoques para mejorar la visualización del grafo.

  L <- layout_with_mds(graph.map,dim=3)                                         # Creamos el layout del grafo.

  rglplot(graph.map,layout = L)                                                 # Dibujamos el grafo en 3D con rgl.
}
