
#Rosenbrock Banana function
banana <- function(xy) {
  x <- xy[1]
  y <- xy[2]
  100*(y - x^2 )^2 + (1 - x)^2
}

#Gradient of banana function
gradient_banana <- function(xy) {
  x <- xy[1]
  y <- xy[2]
  c(-400*x*(y - x^2 ) - 2*(1 - x),
    200*(y - x^2 ))
}


norm <- function(x)
{
  sqrt(sum(x^2))
}

# steepest descent with backtracking
steepest_descent_with_fixed_step <- function(startx,
                                            epsilon=10^-4,
                                            max_iter=10^5)
{
  x <- startx
  iter <- 0

  while(norm(gradient_banana(x)) > epsilon & iter < max_iter) {
    iter <- iter + 1
    gf <- gradient_banana(x)

    # direction, see above for definition of norm
    d <- -gf/norm(gf)

    # fixed step size
    s <- .01

    # update
    x <- x + s*d

  }

  return (list(x=x, error=x-c(1,1), iter=iter))

}

# steepest descent with backtracking
steepest_descent_with_backtrack <- function(startx,
                             epsilon=10^-4,
                              max_iter=10^5)
{
  x <- startx
  iter <- 0

  while(norm(gradient_banana(x)) > epsilon & iter < max_iter) {
    iter <- iter + 1
    gf <- gradient_banana(x)

    # direction, see above for definition of norm
    d <- -gf/norm(gf)

    # step size with backtracking
    s <- 1
    while(banana(x+s*d) > banana(x))
      s <- s/2

    # update
    x <- x + s*d

  }

  return (list(x=x, error=x-c(1,1), iter=iter))

}




