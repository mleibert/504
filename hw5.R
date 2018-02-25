s<-matrix(sample(1:5,25,replace=T),5)
s[lower.tri(s)] = t(s)[lower.tri(s)]

eigen(s)

DD<-diag(eigen(s)[[1]])
DD

Q<-eigen(s)[[2]]

M<-s

qq<-DD%*%t(Q)%*%Q[,1]

qq<-round(qq,15)


Q[1,]*qq

Q%*%qq
M%*%Q[,1]

Q%*%DD%*%t(Q)%*%Q[,1]

t(Q)%*%Q[,1]

m<-sample(1:5,9,replace=T)
m<-matrix(m,ncol=3)
m%*%c(4,3,9)

 DDD<- (DD%*%c(1,0,0,0,0) )

Q%*%DDD

M%*%Q[,1]


eigen(s)[[1]][1]*Q[,1]


###### condition number

  n <- 20
  options(digits=16)

  alpha <- (1:n)/n
  x <- 1:n

  A <- matrix(NA, nrow=n, ncol=n)
  for (i in 1:n) {
    A[,i] <- alpha^(i-1)
  }

  # let's generate b given x.
  b <- A %*% x
  # now let's go back, CAN WE GO BACK?
  xout <- solve(A,b, tol=10^-20)

  output <- matrix(c(x, xout), ncol=2)
  colnames(output) <- c("x", "xout")

  print (list(output=output, kappa=kappa(A),
              log10kappa=log10(kappa(A))))

kappa(B)

kappa(t(B) %*% B )

max( eigen(t(B) %*% B )[[1]] ) / min( eigen(t(B) %*% B )[[1]])


###########

qq=5
qq=matrix(sample(1:100,qq^2),nrow=qq)
qq
pp=matrix(sample(1:100,5 ),nrow=5)
qq%*%pp

norm(qq,"i")
colSums(qq)


diag(5)


####

a<-1;theta=.5*pi

M<-matrix(c(a,a*cos(theta),0,a*sin(theta)),byrow=T,nrow=2)
kappa(M)
det(M)

s<-seq(0,2,.25)
s
w<-seq(-5,5,1)
#w<-w[-6]

DET<-matrix(NA,length(w),length(s))
KAP<-matrix(NA,length(w),length(s))

for(i in 1:length(s) ) { 
for(j in 1:length(w) ) { 

	a<-w[j];theta<-s[i]*pi
	M<-matrix(c(a,a*cos(theta),0,a*sin(theta)),byrow=T,nrow=2)
	DET[j,i]<-det(M)
	KAP[j,i]<-kappa(M)
}
}

