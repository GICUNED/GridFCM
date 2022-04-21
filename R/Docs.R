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

#' @title Types of Propagation Punctions
#'
#' @name propfunctions
#'
#' @description
#' The GridFCM package allows three propagation functions:
#'
#' \strong{Kosko's inference:} It consists of a matrix product between the
#' activation vector and the weight matrix. The new activation of the vertices
#' depends on the activation of their inputs and the weight of their relations.
#' o use it in the function \code{link{fcminfer}} the argument infer must be
#' equal to "k".
#'
#' \strong{Modified Kosko's inference:} It consists of adding the matrix product
#' between the activation vector and the weight matrix to the current state of
#' the vertices. The new activation of the vertices depends on the activation of
#' their inputs, the weight of their relations and the previous state of the
#' network. To use it in the function \code{link{fcminfer}} the parameter infer
#' must be equal to "mk".
#'
#'
#' \strong{Rescalated inference:} Similar to the Modified Kosko but with a
#' rescaling of the values. To use it in the function \code{link{fcminfer}}
#' the parameter infer must be equal to "r".
#'
NULL

#' @title Types of Threshold Function
#' @name thrfunctions
#' @description
#' The GridFCM package allows for four threshold functions, two of which are
#' discrete and the other two are continuous:
#'
#' \strong{Discrete Functions}
#'
#' \emph{\strong{Bivalent function:}} The bivalent function returns a value of 1
#'  to the vertex when the incoming activation is greater than 0, and returns 0
#'  when the incoming activation is less than or equal to 0. This leads to the
#'  vertices taking discrete and dichotomous values (True or false). To use this
#'  function we must write as parameter `thr = "b"` in the function
#'  code{link{fcminfer}}.
#'
#' \emph{\strong{Trivalent function:}} The trivalent function returns a value of
#'  1 to the vertex when the incoming activation is greater than 0, returns 0
#'  when that activation is equal to 0, and returns -1 if that activation is
#'  less than 0. This leads to the vertices to take discrete values, and unlike
#'  bivalent, returns three different possible values; useful for discrete
#'  bipolar dimensions.To use this function, we must write `thr = "tri"` as a
#'  parameter to the function code{link{fcminfer}}.
#'
#' \strong{Continuous Functions}
#'
#' \emph{\strong{Sigmoidal Function:}} The sigmoidal function is the continuous
#' version of the bivalent function. Forces the vertex to be set to an
#' activation range between 0 and 1. If we use this function we must specify an
#' associated lambda which defines the discriminating power of the function;
#' high values of lambda bring the sigmoidal function closer to a bivalent
#' function, low values bring it closer to a linear function. To use this
#' function we must write as parameter `thr = "s"` in the function
#' \code{link{fcminfer}}.
#'
#' \emph{\strong{Hyperbolic Tangent Function:}} The hyperbolic tangent function
#' is the continuos version of the trivalent function. Forces the vertez to be
#' set to an activation range between -1 and +1. If we use this function
#' we must specify an associated lambda which defines the discriminating power
#' of the function; high values of lambda bring the Hyperbolic Tangent Function
#' closer to a trivalent function, low values bring it closer to a linear
#' function. To use this function we must write as parameter `thr = "t"` in the
#' function \code{link{fcminfer}}.
#'
NULL

#' @title Digraph Layouts
#' @name digraphlayouts
#' @description
#' The GridFCM package has five layouts to represent the digraphs:
#'
#' \strong{Graphopt:} Closer two constructs are, more influence they have on
#' each other. To use this layout we must set the layout parameter to "graphopt".
#'
#' \strong{Multidimensional Scaling:} This layout establishes the coordinates of
#' the vertices through a multidimensional scaling of the implication matrix.
#' Closer two constructs are, more similarity there is between them in terms of
#' their implications. To use this layout we have to set the parameter layout to
#' "mds".
#'
#' \strong{Reingold-tilford:} This layout sets the coordinates of the vertices
#' following the algorithm proposed by Reingold and Tilford. To use this layout,
#' we can set the layout parameter to "rtcircle" if we want it in circular
#' format, or to "tree" if we want it in tree format.
#'
#' \strong{Circle:} This layout arbitrarily places the vertices in a circle
#'  without any additional meaning. It is useful to represent a clean image and
#'  avoid overlaps. To use this layout we must set the layout attribute to
#'  "circle".
#'
#' \strong{Grid:} This layout arbitrarily places the vertices in a grid
#'  without any additional meaning. It is useful to represent a clean image and
#'  avoid overlaps. To use this layout we must set the layout attribute to
#'  "grid".
NULL

#' Elsa RepGrid
#'
#' A dataset containing Elsa's RepGrid. From an unpublished case study. You can
#' use it to test the different functions of the package.
#'
#' @docType data
#' @keywords datasets
#' @name elsa2022.grid
#' @usage data(elsa2022)
#' @format A S4 repgrid object
"elsa2022.grid"

#' Elsa ImpGrid
#'
#' A dataset containing Elsa's ImpGrid. From an unpublished case study. You can
#' use it to test the different functions of the package.
#'
#' @docType data
#' @keywords datasets
#' @name elsa2022.imp
#' @usage data(elsa2022)
#' @format A S4 repgrid object
"elsa2022.imp"
