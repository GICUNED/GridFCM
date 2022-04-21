################################################################################
##----------------------------#GRID FUNCTIONS##-------------------------------##
################################################################################

# IMPORT IMPGRID -- importimp()
###############################################################################
#' Import ImpGrid -- importimp()
#'
#' @description Function used to read ImpGrid inside an xlsx file. For the
#' creation of the ImpGrid template you can use the function
#' \code{\link{templateimp}}
#'
#' @param path xlsx file path
#'
#' @param ... This function inherits all the parameters of function
#' \code{\link{importExcel}} from OpenRepGrid package.
#'
#' @return Returns an S4 repgrid object containing the data from the ImpGrid
#'
#' @export
#'

importimp <- function(path,...){

  result <- importExcel(path, ...)                                              # Import ImpGrid xlsx file with importExcel function

  return(result)
}

# IMPORT REPGRID -- importgrid()
################################################################################

#' Import RepGrid -- importgrid()
#'
#' @description Function used to read RepGrid inside an xlsx file.
#'
#' @param path xlsx file path.
#'
#' @param ... This function inherits all the parameters of function
#' \code{\link{importExcel}} from OpenRepGrid package.
#'
#' @return Returns an S4 repgrid object containing the data from the RepGrid.
#'
#' @export

importgrid <- function(path, ...){

  grid <- importExcel(path, ...)                                                # Import RepGrid xlsx file with importExcel function
  grid <- alignByIdeal(grid,dim(grid)[2])                                       # Orient the grid according to the Ideal
  grid <- .alignbyself(grid)                                                    # Orient the grid according to the Self-Now

  return(grid)
}
################################################################################

# EXPORT IMPGRID TEMPLATE -- templateimp()
################################################################################

#' Create ImpGrid Template -- templateimp()
#'
#' @description Function that creates an xlsx file in the working directory.
#' This file contains an impgrid template according to the specified repgrid.
#'
#' @param grid Subject's RepGrid. It must be an S4 object imported by the
#' \code{\link{importgrid}} function.
#'
#' @param name Output file name. Default is "ImpGrid_Template"
#'
#' @return Export an xlsx file in the working directory.
#'
#' @export

templateimp <- function(grid,name ="ImpGrid_Template"){

  if(!requireNamespace("xlsx")){
    stop(cat("ERROR: If you want to use this function you must download xlsx r-package. \n Try typing install.packages('xlsx') in the console \n"))
  }

  dim <- dim(grid)[1]                                                           # Save the number of the constructs

  wb <- xlsx::createWorkbook()
  sh <- xlsx::createSheet(wb)                                                   # Create a workbook and sheet


  fill.izq <- xlsx::CellStyle(wb, fill = xlsx::Fill("#ffff6d","#ffff6d"))
  fill.der <- xlsx::CellStyle(wb, fill = xlsx::Fill("#729fcf","#729fcf"))
  fill.imp <- xlsx::Fill("#77bc65","#77bc65")                                   # Colour of the cells


  dfs <- list(fill.izq,fill.der)
  names(dfs) <- c(1,dim +2)

  rotate.90 <- xlsx::Alignment(rotation = 90)                                   # Text styles.

  self.labels <- getConstructNames(grid)[,1]
  noself.labels <- getConstructNames(grid)[,2]
  imp.labels <- paste(self.labels,"->",noself.labels,sep = " ")                 # Extract the name of the constructs

  m <- matrix(nrow = dim, ncol = dim +2)
  diag(m[1:dim,1:dim+1]) = 0
  df <- data.frame(m)
  df[,1] <- self.labels
  df[,dim + 2] <- noself.labels
  colnames(df) <- c("-3", imp.labels, "3")                                      # Create dataframe with the repgrid data

  xlsx::addDataFrame(df, sh,
               row.names = FALSE,
               colStyle = dfs
               )

  xlsx::addDataFrame(df[1,1:dim +1], sh,
               startColumn = 2,
               row.names = FALSE,
               colnamesStyle = xlsx::CellStyle(wb) + fill.imp + rotate.90
               )                                                                # Stylize the dataframe

  rows <- xlsx::getRows(sh,rowIndex = 1)
  xlsx::setRowHeight(rows,300)
  xlsx::autoSizeColumn(sh,colIndex = c(1:ncol(df)))                             # Adjust cell size

  name <- paste(name,".xlsx",sep = "")                                          # Define filename

  xlsx::saveWorkbook(wb, file = name)                                           # Export xlsx file

  cat("ImpGrid template xlsx file has been successfully created under the name" # Feedback to user
      , name, "in the directory:", getwd())
}
################################################################################
