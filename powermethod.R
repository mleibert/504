simult_iteration <- function(A, V, tol=10^-6)
{
  iter <- 1
  repeat {
    prev_V <- V
    V <- A %*% V
    V <- qr.Q(qr(V))
    
    diff <- as.numeric(V - prev_V)
    
    if (sum(abs(diff)) < tol)
      break
    iter <- iter + 1
  }
  
  # diagnoal entries of V^T A V gives rayleight quotients
  rayleigh <- diag(t(V) %*% A %*% V)
  
  list(V=V, lambda=rayleigh, iterations=iter)
}

 
