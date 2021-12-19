################################################################################
##--------------------#FUZZY COGNITIVE MAPS FUNCTIONS#------------------------##
################################################################################


# ACTIVATION VECTOR -- actvector()
################################################################################

#' Crear Vector de Activación (actvector)
#'
#' @description Función que extrae un vector de un elemento de la rejilla y lo
#' transforma en un vector de activación apropiado para utilizarse a la hora de
#' crear un mapa cognitivo borroso.
#'
#' @param x Rejilla del sujeto desde donde queremos extraer el vector de
#' activación. Debe de ser un objeto de la clase repgrid del paquete
#' OpenRepGrid.
#'
#' @param col.element Elemento desde el que extraemos el vector de activación.
#' Por defecto se establece el primer elemento, que coincide normalmente con el
#' Yo-Actual.
#'
#' @return Devuelve un vector que contiene los pesos para cada uno de los
#' constructos asociados a un elemento.
#'
#' @examples
#'
#' @import OpenRepGrid
#'
#' @export


actvector <- function(x, col.element = 1){

  vector <- getRatingLayer(x)[,col.element]                                     # Extraemos el vector de la rejilla

  result <- (vector - getScaleMidpoint(x))/((getScale(x)[2]-1)/2)               # Transformamos el vector en  un intervalo [-1,1]

  return(result)
}
################################################################################

# FCM INFERENCE -- fcminfer()
################################################################################

#' Inferencia de Escenarios Futuros (fcminfer)
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
#' Puedes mirar la información sobre las diferentes funciones de propagación
#' escribiendo ?\code{\link{PropFunctions}}. Por defecto se utiliza la
#' modificada de Kosko ("mk").
#'
#' @param thr Función Umbral para la inferencia de escenarios. Puedes mirar la
#' información sonbre las distintas funciones umbral escribiendo
#' ?\code{\link{ThrFunctions}}. Por defecto se utiliza la Tangente Hiperbólica
#' ("t").
#'
#' @param lambda Valor de lambda de la función umbral. Solo se aplica si se
#' escoge función umbral sigmoidal o tangente hiperbólica.
#'
#' @param iter Número de iteraciones que se desea inferir
#'
#' @param graph Devuelve un Gráfico de Comportamiento vs. Tiempo cuando es
#' \code{TRUE}
#'
#' @return Devuelve una lista (list) que contiene por filas cada uno de los
#' vectores de escenario en función del número de iteraciones.
#'
#' @examples
#'
#' @import OpenRepGrid
#' @import ggplot2
#'
#' @export

fcminfer <- function(x, imp, act.vec = actvector(x), ideal = dim(x)[2],
                       infer= "mk", thr= "t", lambda = 1 , iter = 30,
                       graph = TRUE){

  w.mat <- .weightmatrix(imp)
  w.mat <- as.data.frame(w.mat)                                                 # Transformamos la matriz de implicaciones en una matriz de pesos



  result <- .infer(act.vec, weight_mat = w.mat, infer = infer,
                      transform = thr, lambda = lambda, iter = iter)            # Aplicamos la función de fcm.infer del paquete fcm para hacer la inferencia


  return(result)
}
################################################################################

# BOT PLOT -- **DEPRECATED**
################################################################################

#'
#' @export

BTplot <- function(x,imp,ideal=dim(x)[2],iter=30){

  lpoles <- getConstructNames(x)[,1]
  rpoles <- getConstructNames(x)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")

  ideal.vector <- OpenRepGrid::getRatingLayer(x)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),
                         nrow = iter, byrow = TRUE)

  res <- fcminfer(x,imp,iter=iter)

  x <- c(1:iter)
  y <- c(0:length(poles))
  y <- as.character(y)
  df <- data.frame(x, abs(res$values - ideal.matrix) / 2)
  colnames(df) <- y


  iterations <- as.numeric(rownames(res$values))
  df <- data.frame(iterations,  abs(res$values - ideal.matrix) / 2)
  df2 <- melt(df, id="iterations")
  ggplotly(ggplot(data=df2,aes(x=iterations, y=value, group=variable,
                           color=variable)) + theme_bw() + geom_line(size=0.7)
       + geom_point(size = 3) + labs(x="Iteraciones",
                                     y="Distancia respecto el ideal",
                                     color="Constructos Personales"))


}

################################################################################
# PERSONAL CONSTRUCTS SYSTEM DYNAMICS PLOT -- pcsd()
################################################################################

#'
#' @export
#' @import plotly

pcsd <- function(x,imp,ideal=dim(x)[2],...){

  lpoles <- OpenRepGrid::getConstructNames(x)[,1]                               # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(x)[,2]
  poles <- paste(lpoles,"-",rpoles,sep = " ")
  iter <- fcminfer(x,imp,iter=60,...)$convergence

  ideal.vector <- OpenRepGrid::getRatingLayer(x)[,ideal]
  ideal.vector <- (ideal.vector - 4)/3
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                        nrow = iter, byrow = TRUE)

  res <- fcminfer(x,imp,iter=iter,...)$values                                   # Obtenemos la inferencia del MCB

  x <- c(0:(iter-1))
  y <- c(0:length(poles))
  y <- as.character(y)
  df <- data.frame(x, abs(res - ideal.matrix) / 2)                              # Confeccionamos un dataframe con las distancias estandarizadas entre los resultados y el ideal
  colnames(df) <- y

  fig <- plot_ly(df, x = ~x, y = df[,2], name = poles[1], type = 'scatter',
                 mode = 'lines+markers',line = list(shape = "spline"))
 for (n in 3:(length(poles)+1)) {
  fig <- fig %>% add_trace(y = df[,n], name = poles[n-1], mode = 'lines+markers'
                           ,line = list(shape = "spline"))
 }
  fig <- fig %>% layout(title="PERSONAL CONSTRUCT SYSTEM DYNAMICS",
                        legend = list(
                          title="PERSONAL CONSTRUCTS"),
                        xaxis = list(
                          title = "ITERATIONS"),
                        yaxis = list(
                          title = "DISTANCE TO IDEAL SELF")
                        )

  fig
}
################################################################################

# FUZZY COGNITIVE MAP DIGRAPH -- fcmdigraph()
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
#' @param results inferencia de escenarios creada con \code{\link{fcminfer}}.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra.
#'
#' @param niter Selección del vector de escenario que se quiere representar en
#' el mapa. Expresado a través del número de iteraciones.
#'
#' @param layout Layout que se quiere utilizar para representar el mapa. Más
#' información de los layouts escribiendo ?\code{\link{GraphLayouts}} o haciendo
#' click sobre él.
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
#' @import igraph
#' @import OpenRepGrid
#'
#' @export


fcmdigraph <- function(x, imp, results = fcminfer(x,imp,graph = FALSE)$values,
                     ideal = dim(x)[2], niter = 30,layout = "graphopt",
                     edge.width = 1.5, vertex.size = 1, legend = FALSE ){

  lpoles <- getConstructNames(x)[,1]
  rpoles <- getConstructNames(x)[,2]                                            # Extraemos los nombres de los polos de los constructos de la Rejilla.

  w.mat <- .weightmatrix(imp)
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
    graph.map <- add_layout_(graph.map,as_tree(circular = TRUE, mode = "out"))
  }
  if(layout == "tree"){
    graph.map <- add_layout_(graph.map,as_tree())
  }
  if(layout == "circle"){
    graph.map <- add_layout_(graph.map,in_circle())
  }
  if(layout == "graphopt"){
    set.seed(3394)
    matrix.seed <- matrix(rnorm(2 * dim(x)[1]), ncol = 2)

    graph.map <- add_layout_(graph.map,with_graphopt(start = matrix.seed))
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


# 3D FUZZY COGNITVE MAP DIGRAPH -- fcmdigraph3D()
################################################################################

#' Digrafo del Mapa Cognitivo Borroso en 3D (fcmdigraph3D)
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
#' @param results inferencia de escenarios creada con \code{\link{fcminfer}}.
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
#'
#' @import igraph
#' @import OpenRepGrid
#'
#' @export
#'
fcmdigraph3D <- function(x, imp, results = fcminfer(x,imp,graph = FALSE)$values,
                       ideal = dim(x)[2], niter=30,edge.width=2,
                       vertex.size =1){

  lpoles <- getConstructNames(x)[,1]
  rpoles <- getConstructNames(x)[,2]                                            # Extraemos los nombres de los polos de los constructos.

  w.mat <- .weightmatrix(imp)
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
################################################################################


# IDEAL FUZZY COGNITIVE MAP DIGRAPH - idealdigraph()
################################################################################
#' Digrafo del Yo-Ideal (IdealMap)
#'
#' @description Función que nos dibuja un digrafo del Ideal del individuo que
#' nos permite ver las implicaciones entre los polos del Yo-Ideal.
#'
#' @param x Rejilla del sujeto del que queremos dibujar el Mapa Cognitivo
#' Borroso. Debe de ser un objeto de la clase repgrid del paquete OpenRepGrid.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importIMP}}.
#'
#' @param inc Si lo establecemos como TRUE nos muestra solamente las relaciones
#' de inconsistencia dentro del ideal.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra.
#'
#' @param layout Layout que se quiere utilizar para representar el mapa.
#'
#' @param edge.width Escalar del grosor de las aristas.
#'
#' @param vertex.size Escalar del tamaño de los vértices.
#'
#' @param legend Dibuja una leyenda con TRUE.
#'
#' @return Devuelve una representación gráfica de un digrafo del Yo-Ideal del
#' individuo
#'
#' @examples
#'
#' @import igraph
#' @import OpenRepGrid
#'
#' @export

idealdigraph <- function(x,imp, ideal = dim(x)[2], inc = FALSE, layout ="circle",
                     edge.width = 1, vertex.size = 1,legend = FALSE){

  lpoles <- getConstructNames(x)[,1]
  rpoles <- getConstructNames(x)[,2]                                            # Extraemos los nombres de los polos de los contrusctos de la Rejilla.

  w.mat <- .weightmatrix(imp)
  w.mat <- as.matrix(w.mat)                                                     # Transformamos las implicaciones en pesos.

  act.vector <- actvector(x,col.element = ideal)
  ideal.results <- fcminfer(x,imp,act.vector,graph = FALSE)$values
  results <- as.numeric(as.data.frame(ideal.results)[1,])                       # Extraemos el vector de escenario seleccionado por el usuario.

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


  if(inc){
    w.mat[w.mat > 0] <- 0
  }


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
  }
                                                                                # Damos grosor a las aristas en función de los pesos.

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

# ANALISYS REPORT -- fcmreport()
################################################################################

#' @export
#'

fcmreport <- function(x, imp, name = "report", dir = getwd()){
  render.grid <- x
  render.imp <- imp
  file <- paste(name,".Rmd", sep = "")
  file.remove(file)

  rmarkdown::draft(name,"informe", package = "GridFCM",edit=FALSE)
  rmarkdown::render(file , output_dir = dir)
  file.remove(file)
}
