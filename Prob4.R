# log likelihood function
logL <- function(alpha, x, y) {
  # -alpha_0 - alpha_1*x_i is used twice, so compute it separately
  alpha_sum <- -alpha[1] - alpha[2]*x
  
  # the (1-y_i)(-alpha_0 - alpha_1*x_i) term
  term1 <- (1-y)*alpha_sum
  
  # the log(1 + exp(-alpha_0 - alpha_1*x_i)) term
  term2 <- log(1 + exp(alpha_sum))
  
  total <- sum(term1 - term2)
  return (total)
}

# gradient of log likelihood
grad_logL <- function(alpha, x, y) {
  # exp(-alpha_0 - alpha_1*x_1) is used several times
  exp_alpha <- exp(-alpha[1] - alpha[2]*x)
  
  # partial with respect to alpha_0
  # -(1-y_i) + exp(-alpha_0 - alpha_1*x_1) /(1+exp(-alpha_0 - alpha_1*x_1))
  p0 <- -(1-y) + exp_alpha/(1 + exp_alpha)
  
  #partial with respect to alpha_1
  # this works out to be x_i times the alpha_0 partial
  p1 <- x*p0
  
  # sum across i
  p0 <- sum(p0)
  p1 <- sum(p1)
  
  return (c(p0, p1))
}


norm <- function(x)
{
  sqrt(sum(x^2))
}


# steepest descent with backtracking
steepest_descent <- function(start_alpha=c(0, 0),
                             epsilon=10^-6,
                              max_iter=3*10^6)
{
  # start timing
  timer <- proc.time()
  
  # load the data
  d <- read.table("o_ring_data.txt", header=T)
  x <- d$Temp
  y <- d$Failure
  
  alpha <- start_alpha
  iter <- 0
  
  # we want to compute the gradient only once per iteration
  current_grad <- grad_logL(alpha, x, y)

  while(norm(current_grad) > epsilon & iter < max_iter) {
    iter <- iter + 1

    # direction, see above for definition of norm
    d <- current_grad/norm(current_grad)

    # step size with backtracking
    s <- 1
    # save value of logL at x so we don't need to recompute in backtracking
    current_logL <- logL(alpha, x, y)
  
    while(logL(alpha+s*d, x, y) < current_logL)
      s <- s/2

    # update
    alpha <- alpha + s*d
    current_grad <- grad_logL(alpha, x, y)

  }
  
  # end timing
  print(proc.time() - timer)

  return (list(alpha=alpha, iter=iter, gradient=current_grad))
}




