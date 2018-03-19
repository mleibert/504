source("NIST.R")

df <- read.table("non_linear_1.txt", header=T)

norm <- function(z)
{
  return (sqrt(sum(z^2)))
}

##############################################################
# Problem 4b
# f(x, b) is the non-linear function we are trying to fit.
# f, grad.f, and hessian.f can be found in the NIST.R script
# in each case x is a scalar and b is an 8-d vector

# Given x value, get the predicted y value given 8 parameters b=(b_1,b_2,..,b_8)
get_predictions  <- function(x, b)
{
  n_samples <- length(x)
  pred_y <- rep(NA, n_samples)
  for (i in 1:n_samples)
    pred_y[i] <- f(x[i], b)

  return (pred_y)
}

# Loss function
# L = \sum_{i=1}^N (y_i - f(x_i, b))^2
L <- function(x, y, b)
{
  pred_y <- get_predictions(x,b)

  # see top of file for definition of norm
  loss <- norm(y - pred_y)
  return (loss)
}

# Gradient of loss function
#grad L = \sum_{i=1}^N -2*(y_i - f(x_i, b)) * grad.f(x_i,b)
grad.L <- function(x, y, b)
{
  pred_y <- get_predictions(x, b)
  nsamples <- length(x)

  total <- 0
  for (i in 1:nsamples) {
    c_grad <- grad.f(x[i], b)
    total <- total - 2*(y[i] - pred_y[i]) * c_grad
  }

  return (total)
}

# Hessian of loss function
#grad L = \sum_{i=1}^N 2 * grad.f(x_i,b) %*% t(grad.f(x_i,b)) - 2*(y_i - f(x_i, b)) * hessian.f(x_i,b)
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

#####################################
# Problem 2c(i)

# below is the function for applying Newton's method.  The function converges in 7
# steps given NIST's suggested start point.  However, for other start points the
# algorithm crashes because the Hessian is singular. 
#
# The function is called using
# Newton()

#' @param x sample x values
#' @param y sample y values
#' @param start_b starting value for parameters, a numeric.vector of length 8
#' @param max.iterations
#' @param tol stop iteration if \|gradient\| <= tol
#'
#' @returns a list with the following elements: (iterations) number of iterations
#' executed, (gradient) the gradient at last iteration, (tol) the norm of the
#' gradient at the last iteration, (solution) value of b at last iteration.
#'
#' @details default values for start_b are taken from the NIST file non_linear_1.txt.
#' Notice that the b[5] and b[8] entries are squared.
#' data.frame df is read from file, see top of file.
Newton <- function(x=df$x, y=df$y,
                   start_b=c(96, .009, 103, 106, 18, 72, 151, 18),
                   max.iterations=10000,
                   tol=1E-5)
{
  iteration <- 0
  b <- start_b

  GL <- grad.L(x, y, b)
  while(iteration < max.iterations & norm(GL) > tol) {
    HL <- hessian.L(x, y, b)
    GL <- grad.L(x, y, b)
    b <- b - solve(HL, GL)

    iteration <- iteration + 1
  }

  return (list(iterations=iteration,
               gradient=GL,
               tol=norm(GL),
               solution=b))
}

#############################################
# Problem 2c(ii)

# below is the function for applying a modified Newton's method.
# in this case checking for conditioning was not needed and I
# didn't include it.

#' see Newton function in 2c(i) above for parameter definitions
NewtonModified <- function(x=df$x, y=df$y,
                   start_b=c(96, .009, 103, 106, 1000, 72, 151, 1000),
                   max.iterations=10000,
                   tol=1E-6)
{

  iteration <- 0
  # create a diagonal matrix for shifting the Hessian
  ID <- diag(rep(1, length(start_b)))
  b <- start_b

  GL <- grad.L(x, y, b)
  while(iteration < max.iterations & norm(GL) > tol) {
    HL <- hessian.L(x, y, b)
    ev_min <- min(eigen(HL)$values)

    # modify the Hessian if necessary
    if (ev_min > 0)
      A <- HL
    else
      A <- HL + (-ev_min + 1)*ID

    # calculate the direction
    GL <- grad.L(x, y, b)
    d <- -solve(A, GL)

    # backtracking should be a separate function, but...
    s <- 1
    while(L(x,y, b) < L(x, y, b + s*d))
      s <- s/2

    b <- b + s*d
    iteration <- iteration + 1

    #cat("executed iteration", iteration, "loss=",
    #    L(x, y, b), "|gradient|=", norm(GL), "s=", s, "\n")
  }

  return (list(iterations=iteration,
               gradient=GL,
               tol=norm(GL),
               solution=b))
}


#########################################################################
# Problem 2c(iv)

# below is the command to exectue nls.   In this case, I treated b5 and b8 as
# the variables, rather than using b5^2, b8^2.   The algorithm converges in 4 steps.
nls_out <- nls(y ~ b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2)
               + b6*exp(-(x-b7)^2/b8^2),
               data=df,
               start=c(b1=96, b2=.009, b3=103, b4=106,
                       b5=18, b6=72, b7=151, b8=18))


