###########################################################
# Define f(x,b) for x scalar and b a 8-d vector:
# f(x, b) = b[1]*exp( -b[2]*x ) + b[3]*exp( -(x-b[4])^2 / b[5]^2) 
#                                   + b[6]*exp( -(x-b[7])^2 / b[8]^2)
#
# We simplify computations by defining two functions phi1 and phi2
#
#     phi1(x, c) = exp(-c*x)
#     phi2(x, v, w) = exp(-(x-v)^2/w^2)
#
# which gives
#
# f(x, b) = b[1]*phi1(x, b[2]) + b[3]*phi2(x, b[4], b[5]) 
#                + b[6]*phi3(x, b[7], b[8])
#
# Below, the following functions assume x as scalar, b as 8-d vector
# f(x, b) = returns value of f.  
# grad.f(x, b) = returns gradient of f relative to b (8-d vector)
# hessian.f(x, b) = returns Hessian of f relative to b (8 by 8 matrix)

# Also included are functions to compute Phi1, Phi2 and their gradients/hessians.  
# These are helper functions used within f, grad.f, hessian.f.

###################################################
# Phi2 functions
#
# We write Phi2 = exp(g(x,v,w)) where g(x,v,w) = -(x-v)^2/w^2
# then 
# grad.Phi2 = grad(g) * Phi2 
# hessian.Phi2 = grad.g %*% t(grad.g) * Phi2 + hessian.g * Phi2

g <- function(x,v,w)
{
  return (-(x-v)^2/(w^2))
}

grad.g <- function(x,v,w)
{
  # gradient relative to v and w
  return (c(2*(x-v)/w^2, 2*(x-v)^2/(w^3)))
}

hessian.g <- function(x,v,w)
{
  # Hessian relative to v and w
  m <- matrix(NA, nrow=2, ncol=2)
  m[1,1] <- -2/(w^2)
  m[1,2] <- -4*(x-v)/(w^3)
  m[2,1] <- m[1,2]
  m[2,2] <- -6*(x-v)^2/(w^4)
  return (m)
}

Phi2 <- function(x,v,w)
{
  return (exp(g(x,v,w)))
}

grad.Phi2 <- function(x,v,w)
{
  return (grad.g(x,v,w)*Phi2(x,v,w))
}

hessian.Phi2 <- function(x,v,w)
{
  Gg <- grad.g(x,v,w)
  Hg <- hessian.g(x,v,w)
  p2 <- Phi2(x,v,w)
  
  return ((Gg %*% t(Gg) + Hg)*p2)
}

####################################################################
# Phi1 functions
# phi1(x, c) = exp(-c*x)

Phi1 <- function(x,c)
{
  return (exp(-c*x))
}

grad.Phi1 <- function(x,c)
{
  return (-x*exp(-c*x))
}

hessian.Phi1 <- function(x,c)
{
  return (x^2*exp(-c*x))
}

####################################################################
# f functions

f <- function(x, b)
{
  term1 <- b[1]*Phi1(x, b[2])
  term2 <- b[3]*Phi2(x, b[4], b[5])
  term3 <- b[6]*Phi2(x, b[7], b[8])
  
  return (term1 + term2 + term3)
}

grad.f <- function(x, b)
{
  # partials[i] is partial of f relative to b[i]
  partials <- rep(NA, 8)
  
  partials[1] <- Phi1(x, b[2])
  partials[2] <- b[1]*grad.Phi1(x, b[2])
  partials[3] <- Phi2(x, b[4], b[5])
  partials[4:5] <- b[3]*grad.Phi2(x, b[4], b[5])
  partials[6] <- Phi2(x, b[7], b[8])
  partials[7:8] <- b[6]*grad.Phi2(x, b[7], b[8])
  
  return (partials)
}

hessian.f <- function(x,b)
{
  H <- matrix(0, nrow=8, ncol=8)
  
  # fill in sub-block for b[1]*Phi1(x, b[2])
  H[2,2] <- b[1]*hessian.Phi1(x, b[2])
  H[2,1] <- grad.Phi1(x, b[2])
  H[1,2] <- H[2,1]
  
  # fill in sub-block for b[3]*Phi2(x, b[4], b[5]^2)
  H[4:5,4:5] <- b[3]*hessian.Phi2(x, b[4], b[5])
  H[3,4:5] <- grad.Phi2(x, b[4], b[5])
  H[4:5,3] <- H[3,4:5]
  
  # fill in sub-block for b[6]*Phi2(x, b[7], b[8]^2)
  H[7:8,7:8] <- b[6]*hessian.Phi2(x, b[7], b[8])
  H[6,7:8] <- grad.Phi2(x, b[7], b[8])
  H[7:8,6] <- H[6,7:8]
  
  return (H)
  
}


