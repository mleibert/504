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
get_alpha0 <- function(eta, i){  return (eta[3*i-2])}

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

Sigma <- function(z)
{
  return (1/(1 + exp(-z)))
}


