##############################################################################
#------------------------------------------#Orientar Rejilla
SwapGrid <- function(x,ideal){
  require(OpenRepGrid)
  n <- 1
  for (rating in getRatingLayer(x)[,ideal]) {
  if(rating < 4){
    x <- swapPoles(x,n)
  }
  n <- n + 1
  }
  return(x)
}
