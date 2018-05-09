 eta<- runif(22,-1,1)
m=4
source("getalpha.r")

 Alpha<-matrix(eta[1:prod( c(dim(X)[2] + 1, m ) )], dim(X)[2] + 1 , m)
  ZZ<-apply(Alpha,2, function(v) sigmoid(v,X))
  head(ZZ)
  
  # first let's compute the Z.  Z will be a N by 4 matrix
  alpha0 <- get_alpha0_vector(eta)
  Aalpha <- get_alpha_matrix(eta)
  
  # the t(t(...)) is a trick to add the alph0 to the rows
  Z_no_sigma <- t(t(X %*% Aalpha) + alpha0)
  Z <- Sigma(Z_no_sigma)  
  head(ZZ);head(Z)
  
all( (round(Z,7)==round(ZZ,7)) == T)
 
sigmoid<-function(a,w){	1/
(1+exp(-a[1]-apply(w,1, function(w)  t(a[-1])%*%w ))) 
}

system.time(
for( i in 1:100){
  Alpha<-matrix(eta[1:prod( c(dim(X)[2] + 1, m ) )], dim(X)[2] + 1 , m)
  apply(Alpha, 2, function(w)   1/(1+exp(- w[1] -   X %*% w[-1]   ))  ) 
  head(ZZ)
})


dim( as.matrix(Alpha[-1,1] ))
dim( t(as.matrix(Alpha[-1,1] )))
dim(X)

apply(Alpha,2,  function(w)

 1/(1+exp(-Alpha[1,w] -   X %*% Alpha[-1,w]   )) 
)






system.time(
for( i in 1:10000){
  # first let's compute the Z.  Z will be a N by 4 matrix
  alpha0 <- get_alpha0_vector(eta)
  Aalpha <- get_alpha_matrix(eta)
  
  # the t(t(...)) is a trick to add the alph0 to the rows
  Z_no_sigma <- t(t(X %*% Aalpha) + alpha0)
  Z <- Sigma(Z_no_sigma)  
  head(ZZ);head(Z)
})




