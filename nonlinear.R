source("NIST.R")

#' Hessian of loss function
#' 
#' @param x - the x coordinates given in the file
#' @param y - the y coordinates given in the file
#' @param b - a numeric vector of length 8, corresponding to b1, b2, .., b8
#' 
#' @return a 8 x 8 matrix that is the Hessian of the Loss function.
#' 
#' @details  HL(b) = \sum_{i=1}^N 2 * grad.f(x_i,b) %*% t(grad.f(x_i,b)) 
#' - 2*(y_i - f(x_i, b)) * hessian.f(x_i,b).  See NIST.R file for the function f.
hessian.L <- function(x, y, b)
{
  pred_y <- get_predictions(x, b)
  nsamples <- length(x)
  
  total <- 0
  for (i in 1:nsamples) {
    c_grad <- grad.f(x[i], b)
    c_hess <- hessian.f(x[i], b)
    
    total <- total + 2 * c_grad %*% t(c_grad)
    total <- total - 2 * (y[i] - pred_y[i]) * c_hess
  }
  
  return (total)
}


##################################
# helper functions to compute the Hessian
norm <- function(z)
{
  return (sqrt(sum(z^2)))
}

# Given x values, get the predicted y values given b
get_predictions  <- function(x, b)
{
  n_samples <- length(x)
  pred_y <- rep(NA, n_samples)
  for (i in 1:n_samples)
    pred_y[i] <- f(x[i], b)
  
  return (pred_y)
}


