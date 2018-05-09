# computes -alpha_0 - \alpha_1 x_1 - ... - \alpha_4 x_4
f <- function(alpha, x)
{
  return (-sum(alpha*c(1,x)))
}

# computes the gradient of f
gradf <- function(alpha, x)
{
  return (-c(1,x))
}

# log likelihood function
logL <- function(alpha, x1, x2, y) {
  
  total <- 0
  n_samples <- length(y)
  for (i in 1:n_samples) {
    f_val <- f(alpha, c(x1[i], x2[i]))
    total <- total + (1-y[i])* f_val - log(1 + exp(f_val)) 
  }
  return (total)
}
 
gradlogL <- function(alpha, x1, x2, y)
{
  # create a vector that will be the gradient
  total <- rep(0, length(alpha))
  
  n_samples <- length(y)
  for (i in 1:n_samples) {
    f_val <- f(alpha, c(x1[i], x2[i]))
    gradf_val <- gradf(alpha, c(x1[i], x2[i]))
    total <- total + (1-y[i]) * gradf_val - exp(f_val)/(1+exp(f_val))*gradf_val
  }
  
  return (total)
}

hesslogL <- function(alpha, x1, x2, y)
{
  # create a matrix that will be the gradient
  total <- matrix(0, nrow=length(alpha), ncol=length(alpha))
  
  n_samples <- length(y)
  for (i in 1:n_samples) {
    f_val <- f(alpha, c(x1[i], x2[i]))
    ratio <- exp(f_val)/(1 + exp(f_val))
    gradf_val <- gradf(alpha, c(x1[i], x2[i]))
    total <- total - ratio^2*gradf_val %*% t(gradf_val)
  }
  
  return (total)
}



DampedNewton <- function(iterations)
{
  d <- read.table("nn.txt", header=T)
  x1 <- d$x1
  x2 <- d$x2
  y <- d$y
  
  # use alpha for eta 
  alpha <- rep(0, 3)
  for (i in 1:iterations) {
    g <- gradlogL(alpha, x1, x2, y)
    H <- hesslogL(alpha, x1, x2, y)
    d <- -solve(H, g)
    
    s <- 1
    while(logL(alpha, x1, x2, y) > logL(alpha + s*d, x1, x2, y)) 
      s <- s/2
    
    alpha <- alpha + s*d
  }
  
  return (alpha)
}

###################################################
# These functions implement the classifier based on alpha
# and compute specificity and sensitivity



# classify points
classify_LR <- function(alpha, p, x1, x2)
{
  n_samples <- length(x1)
  class <- rep(0, n_samples)
  for (i in 1:n_samples) {
    current_sample <- c(x1[i], x2[i])
    current_prob <- 1/(1 + exp(f(alpha, current_sample)))
    if (current_prob >= p)
      class[i] <- 1
  }
  
  return (class)
}

plot_LR_classifications <- function(alpha, p=seq(.1,.91,.1), N=5000)
{
  df <- plyr::adply(p, 1, function(cp) 
    create_LR_classification_df(cp, alpha, N))
  
  gp <- ggplot()
  gp <- gp + geom_point(mapping=aes(x=x1, y=x2, col=y), 
                        data=df, size=1)
  gp <- gp + facet_wrap(~p, nrow=3)
  gp <- gp + xlab(NULL) + ylab(NULL)
  print(gp)
}

# assumes that p is a scalar
create_LR_classification_df <- function(p, alpha, n)
{
  x1 <- 4*runif(n) - 2
  x2 <- 4*runif(n) - 2
 
  y_pred <- classify_LR(alpha, p, x1, x2)
  
  p <- paste("p=", as.character(p), sep="")
  return (data.frame(x1=x1, x2=x2, y=factor(y_pred), p=p, 
                     stringsAsFactors = F))
}

