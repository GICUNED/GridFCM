################################################################################
##--------------------#FUZZY COGNITIVE MAPS FUNCTIONS#------------------------##
################################################################################


# ACTIVATION VECTOR -- actvector()
################################################################################

#' Crear Vector de Activación (actvector)
#'
#' @description Función que extrae un vector de un elemento de la RepGrid y lo
#' estandariza para poder utilizarse como vector de activación un mapa cognitivo
#' borroso.
#'
#' @param grid Rejilla del sujeto desde donde queremos extraer el vector de
#' activación. Debe de ser un objeto de la clase repgrid del paquete
#' OpenRepGrid.
#'
#' @param col.element Elemento desde el que extraemos el vector de activación.
#' Por defecto se establece el primer elemento, que coincide normalmente con el
#' Yo-Actual de la RepGrid.
#'
#' @return Devuelve un vector que contiene los pesos para cada uno de los
#' constructos asociados a un elemento.
#'
#' @import OpenRepGrid
#'
#' @export


actvector <- function(grid, col.element = 1){

  vector <- getRatingLayer(grid)[,col.element]                                  # Extraemos el vector de la rejilla

  result <- (vector - getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)         # Estandarizamos el vector en  un intervalo [-1,1]

  return(result)
}
################################################################################

# FCM INFERENCE -- fcminfer()
################################################################################

#' Inferencia de Escenarios Futuros -- fcminfer()
#'
#' @description Función para inferir escenarios futuros simulados del mapa
#' cognitivo borroso derivados de las relaciones de causalidad.
#'
#' @param grid RepGrid del sujeto desde donde queremos realizar las inferencias.
#' Debe de ser un objeto importando con la función \code{\link{importgrid}}.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param act.vec Vector de activación creado a través de
#' \code{\link{actvector}}. Si no se define se utiliza el vector correspondiente
#'  al primer elemento de la RepGrid.
#'
#' @param ideal Posición del ideal dentro de la RepGrid expresado a través del
#' número de la columna donde se encuentra. Por defecto se establece el último
#' elemento de la RepGrid.
#'
#' @param infer Función de Propagación para la inferencia de escenarios.
#' Puedes mirar la información sobre las diferentes funciones de propagación
#' escribiendo ?\code{\link{PropFunctions}}. Por defecto se utiliza la
#' modificada de Kosko ("mk").
#'
#' @param thr Función de Umbral para la inferencia de escenarios. Puedes mirar
#' la información sonbre las distintas funciones umbral escribiendo
#' ?\code{\link{ThrFunctions}}. Por defecto se utiliza la Tangente Hiperbólica
#' ("t").
#'
#' @param lambda Valor de lambda de la función umbral. Solo se aplica si se
#' escoge función umbral sigmoidal o tangente hiperbólica.
#'
#' @param iter Número de iteraciones que se desea inferir
#'
#' @return Devuelve una lista con dos entradas. La entrada $values contiene por
#' filas cada uno de los vectores de escenario en función del número de
#' iteraciones. Y la entrada $convergence contiene el número de la iteración
#' donde se estabilizada el mapa cognitivo borroso.
#'
#' @import OpenRepGrid
#' @import ggplot2
#'
#' @export

fcminfer <- function(grid, imp, act.vec = actvector(grid), ideal = dim(grid)[2],
                       infer= "mk", thr= "t", lambda = 1 , iter = 30){

  imp_a <- .adaptrepgrid(imp, t = FALSE)                                        # Extraemos los valores de la matriz de implicaciones

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.data.frame(w.mat)                                                 # Transformamos la matriz de implicaciones en una matriz de pesos



  result <- .infer(act.vec, weight_mat = w.mat, infer = infer,
                      transform = thr, lambda = lambda, iter = iter)            # Aplicamos la función para hacer la inferencia

  return(result)
}
################################################################################

# PERSONAL CONSTRUCTS SYSTEM DYNAMICS PLOT -- pcsd()
################################################################################

#' Personal Constructs System Dynamics plot -- pcsd()
#'
#' @description Gráfico que nos permite observar la dinámica del sistema de
#' constructros personales. Cruza la iteraciones matemáticas de
#' \code{\link{fcminfer}} con la distancia respecto al ideal de cada uno de los
#' constructos personales.
#'
#' @param grid RepGrid sobre la que queremos realizar nuestro PCSD. Debe haber
#' sido importada a través de \code{\link{importgrid}}.
#'
#' @param imp Impgrid sobre la que queremos realizar nuestro PCSD. Debe de haber
#' sido importada a través de \code{\link{importimp}}.
#'
#' @param ideal Columna donde se encuentra el Yo-Ideal en la Repgrid. Por
#' defecto se estable la última columna de la Repgrid.
#'
#' @param ... Esta función hereda todos los parámetos de la función
#' \code{\link{fcminfer}}
#'
#' @return Gráfico interactivo confeccionado con plotly.
#'
#' @import plotly
#'
#' @export

pcsd <- function(grid,imp,ideal=dim(grid)[2],...){


  lpoles <- OpenRepGrid::getConstructNames(grid)[,1]                            # Extraemos los nombres de los constructos
  rpoles <- OpenRepGrid::getConstructNames(grid)[,2]
  poles <- paste(lpoles,"—",rpoles,sep = "")

  iter <- fcminfer(grid,imp,iter=60,...)$convergence                            # Establecemos el número de iteración donde se estabiliza el FCM

  ideal.vector <- OpenRepGrid::getRatingLayer(grid)[,ideal]
  ideal.vector <- (ideal.vector -
                   getScaleMidpoint(grid))/((getScale(grid)[2]-1)/2)
  ideal.matrix <- matrix(ideal.vector, ncol = length(ideal.vector),             # Creamos una matriz con los valores del yo-ideal repetidos por filas
                        nrow = iter, byrow = TRUE)

  res <- fcminfer(grid,imp,iter=iter,...)$values                                # Obtenemos la inferencia del MCB

  x <- c(0:(iter-1))
  y <- c(0:length(poles))
  y <- as.character(y)
  df <- data.frame(x, abs(res - ideal.matrix) / 2)                              # Confeccionamos un dataframe con las distancias estandarizadas entre los resultados y el ideal
  colnames(df) <- y

  fig <- plot_ly(df, x = ~x, y = df[,2], name = poles[1], type = 'scatter',
                 mode = 'lines+markers',line = list(shape = "spline"))          # Construimos el gráfico de plotly de las iteraciones para cada uno de los constructos
 for (n in 3:(length(poles)+1)) {
  fig <- fig %>% add_trace(y = df[,n], name = poles[n-1], mode = 'lines+markers'
                           ,line = list(shape = "spline"))
 }
  fig <- fig %>% layout(title="PERSONAL CONSTRUCT SYSTEM DYNAMICS",
                        xaxis = list(
                          title = "ITERATIONS"),
                        yaxis = list(
                          title = "DISTANCE TO IDEAL SELF",
                          range = c(-0.05,1.05))
                        )
  fig <- fig %>% layout(legend=list(
                          title=list(text='<b>PERSONAL CONSTRUCTS</b>')))

  fig                                                                           # Ejecutamos el gráfico
}
################################################################################

# FUZZY COGNITIVE MAP DIGRAPH -- fcmdigraph()
################################################################################

#' Fuzzy Cognitive Map Digraph -- fcmdigraph()
#'
#' @description Función que nos dibuja un digrafo del Mapa Cognitivo Borroso del
#' sistema de constructos de un individuo a través de una técnica de rejilla y
#' una rejilla de implicaciones.
#'
#' @param grid RepGrid del sujeto del que queremos dibujar el Mapa Cognitivo
#' Borroso. Debe de ser un objeto importado con la función
#' \code{\link{importgrid}}.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param results inferencia de escenarios creada con \code{\link{fcminfer}}.
#' Por defecto se establece una inferencia sobre el Yo-Actual.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra. Por defecto el último elemento de la
#' RepGrid
#'
#' @param niter Selección del vector de escenario que se quiere representar en
#' el mapa. Expresado a través del número de iteraciones. Por defecto se
#' establece la primera iteracion.
#'
#' @param layout Layout que se quiere utilizar para representar el mapa. Más
#' información de los layouts escribiendo ?\code{\link{GraphLayouts}}.
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
#'
#' @import igraph
#' @import OpenRepGrid
#'
#' @export


fcmdigraph <- function(grid, imp, results = fcminfer(grid,imp)$values,
                     ideal = dim(grid)[2], niter = 1,layout = "graphopt",
                     edge.width = 1.5, vertex.size = 1, legend = FALSE ){

  imp_a <- .adaptrepgrid(imp, t = FALSE)                                        # Extraemos los valores de la matriz de implicaciones

  lpoles <- getConstructNames(grid)[,1]
  rpoles <- getConstructNames(grid)[,2]                                         # Extraemos los nombres de los polos de los constructos de la RepGrid

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.matrix(w.mat)                                                     # Transformamos las implicaciones en pesos

  results <- as.numeric(as.data.frame(results)[niter,])                         # Extraemos el vector de escenario seleccionado por el usuario de la matriz de iteraciones.


  n <- 1                                                                         # Orientamos la matriz de pesos en funcíon del estado actual de la matriz
                                                                                 # Esto sirve para cambiar el color de las aristas en función del estado de los vértices
  for (integer in results) {
    if(integer != 0){
    integer.value <- integer / abs(integer)
    w.mat[,n] <- w.mat[,n] * integer.value
    w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }


  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Creamos un gráfico primitivo y poco a poco lo iremos configurando

  E(graph.map)$color <- "black"
  n <- 1                                                                        # Damos color a las aristas en función del tipo de relación
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
    n <- n + 1
  }

  edge.curved <- rep(0, length(E(graph.map)))                                   # Damos curvatura a las aristas para evitar superposiciones en los casos de bicausalidad
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
  }

  n <- 1                                                                        # Damos grosor a las aristas en función de los pesos.
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[N,M] != 0){
        E(graph.map)$width[n] <- abs(edge.width * w.mat[N,M])
        n <- n + 1
      }
    }
  }

  V(graph.map)$color <- "black"
  n <- 1                                                                        # Damos color a los vértices en función de su orientación al ideal.
  for (pole.vertex in results) {
  if(getRatingLayer(grid)[,ideal][n] > 4){
    if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
    else{
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
      else{
        if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
      }
    }
  }
    if(getRatingLayer(grid)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] == 4){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }

  n <- 1                                                                        # Damos nombre a los vértices en función del polo que se encuentra activado.
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"—",
                                                                rpoles[n],sep =
                                                                  "")}
      }
    }
    n <- n + 1
  }

  V(graph.map)$size <- 1
  n <- 1                                                                        # Damos tamaño a los vértices en función de su grado de activación.
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
    n <- n + 1
  }
                                                                                # Realizamos un serie de retoques finales para mejorar la visualización.
  E(graph.map)$arrow.size <- edge.width * 0.3
  V(graph.map)$shape <- "circle"
  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"

                                                                                # Detectamos y aplicamos el layout escogido
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
    matrix.seed <- matrix(rnorm(2 * dim(grid)[1]), ncol = 2)

    graph.map <- add_layout_(graph.map,with_graphopt(start = matrix.seed))
  }
  if(layout == "mds"){
    graph.map <- add_layout_(graph.map,with_mds())
  }
  if(layout == "grid"){
    graph.map <- add_layout_(graph.map,on_grid())
  }

  plot.igraph(graph.map, edge.curved = edge.curved)                             # Ejecutamos el gráfico

  if(legend){                                                                   # Dibujamos la leyenda del mapa cognitivo borroso.
    poles <- paste(lpoles,rpoles,sep = " - ")
    poles <- paste(c(1:length(poles)),poles,sep = ". ")
    legend("topright",legend = poles, cex = 0.7,
           title = "Constructos Personales")
  }
}
################################################################################


# 3D FUZZY COGNITVE MAP DIGRAPH -- fcmdigraph3D()
################################################################################

#' 3D Fuzzy Cognitive Digraph -- fcmdigraph3D()
#'
#' @description Función que nos dibuja un digrafo del Mapa Cognitivo Borroso en
#' tres dimensiones del sistema de constructos de un individuo a través de una
#' técnica de rejilla y una rejilla de implicaciones. Utilizando un layout de
#' escalado multidimensional.
#'
#' @param grid RepGrid del sujeto del que queremos dibujar el Mapa Cognitivo
#' Borroso. Debe de ser un objeto importado con la función
#' \code{\link{importgrid}}.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param results inferencia de escenarios creada con \code{\link{fcminfer}}.
#' Por defecto se establece una inferencia sobre el Yo-Actual.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra. Por defecto se estable el último
#' elemento de la Repgrid.
#'
#' @param niter Selección del vector de escenario que se quiere representar en
#' el mapa. Expresado a través del número de iteracion. Por defecto se
#' establece la primera iteración.
#'
#' @param edge.width Escalar del grosor de las aristas.
#'
#' @param vertex.size Escalar del tamaño de los vértices.
#'
#' @return Devuelve una representación gráfica de un digrafo de un Mapa
#' Cognitivo Borroso en un escalado multidimensional de 3 dimensiones.
#'
#'
#'
#' @import igraph
#' @import OpenRepGrid
#'
#' @export
#'

fcmdigraph3D <- function(grid, imp, results = fcminfer(grid,imp)$values,
                       ideal = dim(grid)[2], niter=1, edge.width=2,
                       vertex.size =1){

  imp_a <- .adaptrepgrid(imp, t = FALSE)                                        # Extraemos los valores de la matriz de implicaciones.

  lpoles <- getConstructNames(grid)[,1]
  rpoles <- getConstructNames(grid)[,2]                                            # Extraemos los nombres de los polos de los constructos.

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.matrix(w.mat)                                                     # Transformamos la matriz de implicaciones en una matriz de pesos

  results <- as.numeric(as.data.frame(results)[niter,])                         # Extraemos el vector de escenario que quiere representar el usuario

                                                                                # Orientamos la matriz de pesos en funcíon del estado actual de la matriz.
  n <- 1                                                                        # Esto sirve para cambiar el color de las aristas en función del estado de los vértices.
  for (integer in results) {
    if(integer != 0){
      integer.value <- integer / abs(integer)
      w.mat[,n] <- w.mat[,n] * integer.value
      w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }

  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Creamos el grafo primitivo y lo vamos configurando poco a poco

  V(graph.map)$size <- 1

  n <- 1                                                                        # Damos tamaño a los vértices en función de su grado de activación
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
    n <- n + 1
  }

  n <- 1                                                                        # Damos nombre a los vértices según que polo se encuentre activado
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"—",
                                                                rpoles[n],sep =
                                                                  "")}
      }
    }
    n <- n + 1
  }

  V(graph.map)$color <- "black"                                                 # Damos color a los vértices según su orientación al ideal.
  n <- 1
  for (pole.vertex in results) {
    if(getRatingLayer(grid)[,ideal][n] > 4){
      if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] == 4){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }

  E(graph.map)$color <- "black"                                                 # Damos color a las aristas en función del tipo de causalidad.
  n <- 1
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )
    n <- n + 1
  }

  V(graph.map)$label.cex <- 0.75                                                # Hacemos unos últimos retoques para mejorar la visualización del grafo.
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"
  V(graph.map)$label.dist <- 1.5

  L <- layout_with_mds(graph.map,dim=3)                                         # Creamos el layout del grafo.

  rglplot(graph.map,layout = L)                                                 # Dibujamos el grafo en 3D con rgl.
}
################################################################################


# IDEAL FUZZY COGNITIVE MAP DIGRAPH -- idealdigraph()
################################################################################
#' Ideal Map Digraph -- idealdigraph()
#'
#' @description Función que nos dibuja un digrafo del Ideal del individuo que
#' nos permite ver las implicaciones entre los polos del Yo-Ideal.
#'
#' @param grid RepGrid del sujeto del que queremos dibujar el Mapa Cognitivo
#' Borroso. Debe de ser un objeto importado con la función
#' \code{\link{importgrid}}.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param inc Si lo establecemos como TRUE nos muestra solamente las relaciones
#' de inconsistencia (relaciones inversas) dentro del ideal.
#'
#' @param ideal Posición del ideal dentro de la rejilla expresado a través del
#' número de la columna donde se encuentra. Por defecto se establece el último
#' elemento de la rejilla.
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
#'
#' @import igraph
#' @import OpenRepGrid
#'
#' @export

idealdigraph <- function(grid,imp, ideal = dim(grid)[2], inc = FALSE,
                         layout ="circle", edge.width = 1, vertex.size = 1,
                         legend = FALSE){

  act.vector <- actvector(grid,col.element = ideal)
  ideal.results <- fcminfer(grid,imp,act.vector)$values

  imp_a <- .adaptrepgrid(imp, t = FALSE)

  lpoles <- getConstructNames(grid)[,1]                                         # Extraemos los nombres de los polos de los contructos de la RepGrid.
  rpoles <- getConstructNames(grid)[,2]

  w.mat <- .weightmatrix(imp_a)
  w.mat <- as.matrix(w.mat)                                                     # Transformamos las implicaciones en pesos.


  results <- as.numeric(as.data.frame(ideal.results)[1,])                       # Extraemos el vector de escenario seleccionado por el usuario.

  n <- 1
  for (integer in results) {                                                    # Orientamos la matriz de pesos en funcíon del estado actual de la matriz.
    if(integer != 0){                                                           # Esto sirve para cambiar el color de las aristas en función del estado de los vértices
      integer.value <- integer / abs(integer)
      w.mat[,n] <- w.mat[,n] * integer.value
      w.mat[n,] <- w.mat[n,] * integer.value
    }
    n <- n + 1
  }



  if(inc){
    w.mat[w.mat > 0] <- 0
  }


  graph.map <- graph.adjacency(w.mat,mode = "directed",weighted = T)            # Creamos un gráfico primitivo y poco a poco lo vamos configurando.

  E(graph.map)$color <- "black"
  n <- 1
  for (weight in E(graph.map)$weight) {
    E(graph.map)$color[n] <-  ifelse(weight < 0, "red", "black" )               # Damos color a las aristas en función del tipo de relación.
    n <- n + 1
  }

  edge.curved <- rep(0, length(E(graph.map)))
  n <- 1                                                                        # Damos curvatura a las aristas para evitar superposiciones en los casos de bicausalidad.
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[M,N] != 0 && w.mat[N,M] != 0){
        edge.curved[n] <- 0.25
      }
      if(w.mat[N,M] != 0){
        n <- n + 1

      }
    }
  }

  n <- 1                                                                        # Damos grosor a las aristas en función de los pesos de las implicaciones.
  for (N in 1:dim(w.mat)[1]) {
    for (M in 1:dim(w.mat)[1]) {
      if(w.mat[N,M] != 0){
        E(graph.map)$width[n] <- abs(edge.width * w.mat[N,M])
        n <- n + 1
      }
    }
  }


  V(graph.map)$color <- "black"
  n <- 1                                                                        # Damos color a los vértices en función de su orientación al ideal.
  for (pole.vertex in results) {
    if(getRatingLayer(grid)[,ideal][n] > 4){
      if(pole.vertex < 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex > 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] < 4){
      if(pole.vertex > 0){V(graph.map)$color[n] <- "#F52722" }
      else{
        if(pole.vertex < 0){V(graph.map)$color[n] <- "#a5d610" }
        else{
          if(pole.vertex == 0){V(graph.map)$color[n] <- "grey" }
        }
      }
    }
    if(getRatingLayer(grid)[,ideal][n] == 4){
      V(graph.map)$color[n] <- "yellow"
    }
    n <- n + 1
  }

  n <- 1                                                                        # Damos nombre a los vértices en función del polo que se encuentra activado.
  for (pole.name.vertex in results) {
    if(pole.name.vertex < 0){V(graph.map)$name[n] <- lpoles[n] }
    else{
      if(pole.name.vertex > 0){V(graph.map)$name[n] <- rpoles[n] }
      else{
        if(pole.name.vertex == 0){V(graph.map)$name[n] <- paste(lpoles[n],"—",
                                                                rpoles[n],sep =
                                                                  "")}
      }
    }
    n <- n + 1
  }

  V(graph.map)$size <- 1
  n <- 1                                                                        # Damos tamaño a los vértices en función de su grado de activación.
  for (size.vertex in results) {
    size.vertex <- abs(size.vertex)
    V(graph.map)$size[n] <-  vertex.size * (5 + size.vertex * 15)
    n <- n + 1
  }

  E(graph.map)$arrow.size <- edge.width * 0.3                                   # Realizamos un serie de retoques finales para mejorar la visualización.
  V(graph.map)$shape <- "circle"
  V(graph.map)$label.cex <- 0.75
  V(graph.map)$label.family <- "sans"
  V(graph.map)$label.font <- 2
  V(graph.map)$label.color <- "#323232"


  if(layout == "rtcircle"){                                                     # Configuramos el layout a mostrar.
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
  }

  plot.igraph(graph.map, edge.curved = edge.curved)                             # Ejecutamos el gráfico.

  if(legend){                                                                   # Dibujamos la leyenda
    poles <- paste(lpoles,rpoles,sep = " - ")
    poles <- paste(c(1:length(poles)),poles,sep = ". ")
    legend("topright",legend = poles, cex = 0.7,
           title = "Constructos Personales")
  }
}
################################################################################

# ANALISYS REPORT -- fcmreport()
################################################################################


#' GridFCM Analisys Report -- fcmreport())
#'
#' @description Función que nos exporta un análisis completo sobre un sujeto
#' utilizando
#'
#' @param grid RepGrid del sujeto del que queremos dibujar el Mapa Cognitivo
#' Borroso. Debe de ser un objeto importado con la función
#' \code{\link{importgrid}}.
#'
#' @param imp Matriz de implicaciones del sujeto importada con
#' \code{\link{importimp}}.
#'
#' @param name Nombre del archivo de salida
#'
#' @param dir Directorio dónde se va a crear el archivo de salida. Por defecto
#' se estable el directorio de trabajo.
#'
#' @param output Work in progress
#'
#' @param edit Parametro booleano para indicar si se desea editar el código
#' fuente del informe. Por defecto se encuentra definido como FALSE.
#'
#' @return Devuelve un archivo html que muestra un análisis completo sobre la
#' RepGrid y la ImpGrid del sujeto.
#'
#' @import rmarkdown
#'
#' @export
#'

fcmreport <- function(x, imp, name = "report", dir = getwd(), output = "html",
                      edit = FALSE){

  render.grid <- x                                                              # Creamos los objetos renderizables para el draft
  render.imp <- imp

  file <- paste(name,".Rmd", sep = "")                                          # Damos nombre al archivo de salida

  file.remove(file)                                                             # Eliminamos posibles archivos residuales de algún error previo de la función
  file.remove("style.css")
  file.remove("gridfcm.png")

if(output == "html"){                                                           # Creamos el draft del html y lo renderizamos.
  rmarkdown::draft(name,"report_html", package = "GridFCM",edit=edit)
  rmarkdown::render(file , output_dir = dir)
}
if(output =="shiny"){
  rmarkdown::draft(name,"report_shiny", package = "GridFCM",edit=edit)         # Creamos el draft del Shiny y lo ejecutamos
  rmarkdown::run(file, shiny_args = list(launch.browser = TRUE))
}
  file.remove(file)
  file.remove("style.css")
  file.remove("gridfcm.png")                                                    # eliminamos los archivos temporales de la renderización
}
