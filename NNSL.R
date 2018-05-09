# this function checks if the get functions below work
# I called it and the gets worked properly.
check_get <- function(eta)
{
  eta = 1:22
  # let's build eta from the gets and see if we get back
  # eta
  eta_test <- c()
  for (i in 1:4) {
    eta_test <- c(eta_test, get_alpha0(eta, i), 
                  get_alpha(eta,i))
  }
  for (i in 1:2) {
    eta_test <- c(eta_test, get_beta0(eta, i), 
                  get_beta(eta,i))
  }
  
  print("true eta")
  print(eta)
  print("computed eta")
  print(eta_test)
}

# return alpha_0^i
get_alpha0 <- function(eta, i)
{
  return (eta[3*i-2])
}

# collect all alpha0 as a vector
get_alpha0_vector <- function(eta)
{
  vec <- rep(0, 4)
  for (i in 1:4)
    vec[i] <- get_alpha0(eta, i)
  
  return (vec)
}

# return beta_0^i
get_beta0 <- function(eta, i)
{
  return (eta[5*i-4 + 12])
}

# return alpha^i
get_alpha <- function(eta, i)
{
  return (c(eta[3*i-1], eta[3*i]))
}

# collect all alpha0 as a vector
get_beta0_vector <- function(eta)
{
  vec <- rep(0, 2)
  for (i in 1:2)
    vec[i] <- get_beta0(eta, i)
  
  return (vec)
}

# for fast computation in NN, form a matrix with the alphas as columns
get_alpha_matrix <- function(eta)
{
  # a bit of a hack, but...
  all_alphas <- c(get_alpha(eta, 1), get_alpha(eta, 2),
                  get_alpha(eta, 3), get_alpha(eta, 4))
  m <- matrix(all_alphas, ncol=4)
  return(m)
}

get_beta_matrix <- function(eta)
{
  # a bit of a hack, but...
  all_betas <- c(get_beta(eta, 1), get_beta(eta, 2))
  m <- matrix(all_betas, ncol=2)
  return(m)
}

# return beta^i
get_beta <- function(eta, i)
{
  return (c(eta[5*i-3 + 12], 
            eta[5*i-2 + 12], 
            eta[5*i-1 + 12], 
            eta[5*i + 12]))
}


sigma <- function(z)
{
  return (1/(1 + exp(-z)))
}

# Here is a non-vectorized function to compute the NN, which runs slow
# but with simple code. I didn't use this in the solutions.
NN_non_vector <- function(eta, x)
{
  # first let's compute the Z's
  Z <- rep(0, 4)
  for (i in 1:4) {
    alpha0 <- get_alpha0(eta, i)
    alpha <- get_alpha(eta, i)
    Z[i] <- sigma(alpha0 + sum(alpha*x))
  }
  
  # compute the T's, T is used in R for TRUE, so I use TT,
  # although using T will work as well.
  TT <- rep(0, 2)
  for (i in 1:2) {
    beta0 <- get_beta0(eta, i)
    beta <- get_beta(eta, i)
    TT[i] <- sigma(beta0 + sum(beta*Z))
  }
  
  
  # compute the Y's
  Y <- rep(0, 2)
  denom <- exp(TT[1]) + exp(TT[2])
  Y[1] <- exp(TT[1])/denom
  Y[2] <- exp(TT[2])/denom
  
  return (Y)
}

# Here's a vectorized function to compute the neural net.
NN <- function(eta, X)
{
  # first let's compute the Z.  Z will be a N by 4 matrix
  alpha0 <- get_alpha0_vector(eta)
  alpha <- get_alpha_matrix(eta)
  
  # the t(t(...)) is a trick to add the alph0 to the rows
  Z_no_sigma <- t(t(X %*% alpha) + alpha0)
  Z <- sigma(Z_no_sigma)
  
  # compute the T's, T is used in R for TRUE, so I use TT
  # TT will be an N by 2 matrix
  beta0 <- get_beta0_vector(eta)
  beta <- get_beta_matrix(eta)
  TT_no_sigma <- t(t(Z %*% beta) + beta0)
  TT <- sigma(TT_no_sigma)
  
  # compute the Y's
  denom <- rowSums(exp(TT))
  Y <- exp(TT)/denom
 
  return (Y)
}

#####################################################
# likelihood functions
logL <- function(eta, X, y, rho)
{
  Y <- NN(eta, X)
  logvals <- ifelse(y == 1, log(Y[,2]), log(Y[,1]))
  logL <- sum(logvals) - rho*sum(eta*eta)
  
  return (logL)
}

grad_logL <- function(eta, X, y, rho)
{
  g <- rep(0, 22)
  base_value <- logL(eta, X, y, rho)
  for (i in 1:22) {
    eta_new <- eta
    eta_new[i] <- eta_new[i] + 10^-7
    g[i] <- logL(eta_new, X, y, rho) - base_value
    g[i] <- g[i]/(10^-7)
  }
  
  return (g)
}

##################################################
norm <- function(z)
{
  return (sqrt(sum(z^2)))
}
# steepest ascent algorithm to train the neural net
# X is the matrix of the samples
train_NN <- function(start_eta, iterations, 
                     rho, print_modulus=500,
                     file="nn.txt", backtrack=T)
{
  time_it <- proc.time()
  
  d <- read.table(file, header=T)
  X <- as.matrix(d[,1:2])
  y <- d$y
  
  eta <- start_eta
  for (i in 1:iterations) {
    d <- grad_logL(eta, X, y, rho=rho)
    d <- d/norm(d)
    
    s <- 1
    if (backtrack)
      while(logL(eta, X, y, rho=rho) > logL(eta+s*d, X, y, rho=rho))
         s <- s/2
    
    eta <- eta + s*d
    
    # for timing/debugging
    # let's print the log likelihood every 100th iteration
    #if (i %% print_modulus == 0)
    #  cat("iteration", i, ", log likelihood = "
    #      , logL(eta, X, y, rho), "\n")
    
  }
  
  # show time, for setup..
  #print(proc.time() - time_it)
  
  return (eta)
}


########################################################
# given an alpha and p, predict the class of each sample
classify_NN <- function(eta, p, X)
{
  Y2 <- NN(eta, X)
  class <- ifelse(Y2[,2] >= p , 1, 0)
  
  return (class)
}

