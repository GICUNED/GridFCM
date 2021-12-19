################################################################################
##----------------------------#HIDE FUNCTIONS##-------------------------------##
################################################################################

# REPGRID TO MULTIVARIATE MATRIX -- .adaptrepgrid()
################################################################################
.adaptrepgrid <- function(x, t=FALSE){

  result <- getRatingLayer(x)

  if(!t){
  result <- t(result)
  }

  return(result)
}
################################################################################

# ALIGN BY SELF --  .alignbyself()
################################################################################
.alignbyself <- function(x, self = 1){
  for (i in 1:dim(x)[1]) {
    if(getRatingLayer(x)[i,self] > 4){
      x <- swapPoles(x,i)
    }
  }
  return(x)
}
################################################################################

# WEIGHT MATRIX -- .weightmatrix()
################################################################################

.weightmatrix <- function(x){
  result <- x/3
  return(result)
}
################################################################################

# HIDE FCM INFER -- .infer()
################################################################################

.infer <- function (activation_vec, weight_mat, iter = 20, infer = "k",
                    transform = "s", lambda = 1, e = 0.001)
{
  if (length(which(activation_vec > 1)) & length(which(activation_vec >
                                                       -1))) {
    stop("Please check the concepts' values of the activation vector. They must be in the range -1 and 1.")
  }
  if (length(which(weight_mat > 1)) & length(which(weight_mat >
                                                   -1))) {
    stop("Please check the weights of the matrix. They must be in the range -1 and 1.")
  }
  if (sum(is.na(activation_vec)) > 0) {
    stop("Please check the activation vector for missing values.")
  }
  if (sum(is.na(weight_mat)) > 0) {
    stop("Please check the weight matrix for missing values.")
  }
  if (iter <= 0)
    stop("The iterations must be higher than zero.")
  if (sum(!(infer %in% c("k", "mk", "r", "kc", "mkc", "rc"))) >
      0)
    stop("For the Inference Rule only Kosko 'k', modified Kosko 'mk',  Rescale 'r', Kosko-clamped 'kc', modified Kosko-clamped 'mkc' or Rescale-clamped 'rc' variables are allowed.")
  if (sum(!(transform %in% c("b", "tr", "s", "t"))) > 0)
    stop("For the transformation functions only Bivalent 'b', Trivalent 'tr', Sigmoid 's' or\n            Hyperbolic tangent 't' variables are allowed.")
  if ((lambda <= 0) || (lambda >= 10))
    stop("Lambda value should be in the range 1 to 10.")
  if ((e < 1e-06) || (e > 0.01))
    stop("Epsillon (e) value should be in the range 0.01 to 0.000001.")
  m <- ncol(weight_mat)
  mylist <- list()
  for (i in 1:(iter - 1)) {
    if (i == 1) {
      if (infer == "k" || infer == "kc") {
        initial_vec <- colSums(t(activation_vec) * weight_mat)
      }
      else if (infer == "mk" || infer == "mkc") {
        initial_vec <- activation_vec + colSums(t(activation_vec) *
                                                  weight_mat)
      }
      else if (infer == "r" || infer == "rc") {
        initial_vec <- (2 * activation_vec - 1) + colSums(t((2 *
                                                               activation_vec) - 1) * weight_mat)
      }
      if (transform == "s") {
        initial_vec <- 1/(1 + exp(-lambda * initial_vec))
      }
      if (transform == "t") {
        initial_vec <- tanh(lambda * initial_vec)
      }
    }
    else {
      if (infer == "k" || infer == "kc") {
        initial_vec <- colSums(t(initial_vec) * weight_mat)
      }
      else if (infer == "mk" || infer == "mkc") {
        initial_vec <- initial_vec + colSums(t(initial_vec) *
                                               weight_mat)
      }
      else if (infer == "r" || infer == "rc") {
        initial_vec <- (2 * initial_vec - 1) + colSums(t((2 *
                                                            initial_vec) - 1) * weight_mat)
      }
      if (transform == "s") {
        initial_vec <- 1/(1 + exp(-lambda * initial_vec))
      }
      if (transform == "t") {
        initial_vec <- tanh(lambda * initial_vec)
      }
    }
    if (transform == "b") {
      for (j in 1:m) {
        if (initial_vec[j] > 0) {
          initial_vec[j] <- 1
        }
        else if (initial_vec[j] <= 0) {
          initial_vec[j] <- 0
        }
      }
    }
    if (transform == "tr") {
      for (j in 1:m) {
        if (initial_vec[j] > 0) {
          initial_vec[j] <- 1
        }
        else if (initial_vec[j] < 0) {
          initial_vec[j] <- -1
        }
        else initial_vec[j] <- 0
      }
    }
    if (infer == "kc" || infer == "mkc" || infer == "rc") {
      for (k in 1:m) {
        if (activation_vec[k] == 1) {
          initial_vec[k] <- (initial_vec[k] = 1)
        }
      }
    }
    mylist[[i]] <- initial_vec
  }
  steps_t <- (as.data.frame(do.call("rbind", mylist)))
  step_1 <- as.numeric(activation_vec)
  A <- (rbind(step_1, steps_t))
  last_conv <- as.double(A[iter, ] - A[(iter - 1), ])
  Res_e <- (length(last_conv[last_conv <= e]))
  if (Res_e < m) {
  }
  else {
    mylist1 <- list()
    for (i in 2:(iter)) {
      subst <- abs(apply(A, 2, function(x) x[i] - x[i -
                                                      1]))
      mylist1[[i]] <- subst
    }
    subst.mat <- do.call("rbind", mylist1)
    w <- as.data.frame(matrix(e, (iter - 1), m))
    mylist3 <- list()
    for (i in 1:(iter - 1)) {
      if (all(subst.mat[i, ] < w[i, ])) {
        cv <- 1
      }
      else {
        cv <- 2
      }
      mylist3[[i]] <- cv
    }
    cv.mat <- do.call("rbind", mylist3)
    if (cv.mat[[i]] == 2) {
    }
    else {
      conv_state <- min(which(cv.mat == 1))
    }
  }
  outlist <- list(values = A)
  outlist$convergence <- Res_e
  return(outlist)
}

