setwd("G:\\math\\504")
require(ggplot2,quietly=T)

nn<-read.table("nn.txt",header=T)	
X<-as.matrix(nn[,-3])
p <- ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y))) +   
  theme(legend.position="bottom")
p 


sigmoid<-function(a,w){	(1+exp(-a[1]-apply(w,1, 
	function(w)  t(a[-1])%*%w )))^(-1)}

NNml<-function(x,eta,m){
	
	X<-as.matrix(x)

	Alpha<-matrix(eta[1:prod( c(dim(X)[2] + 1, m ) )], dim(X)[2] + 1 , m)
	#Z<-apply(Alpha,2, function(v) sigmoid(v,X))
	Z<-apply(Alpha, 2, function(w) 1/(1+exp(- w[1] -   X %*% w[-1]  )) ) 

	Beta<-matrix( eta[(prod(dim(Alpha))+1):length(eta) ], dim(Z)[2]+1, 2)
	 
	#TT<-apply(Beta,2, function(v) sigmoid(v,Z))
	TT<-apply(Beta, 2, function(w) 1/(1+exp(- w[1] - Z %*% w[-1]  )) ) 
	Y<- apply(TT,2, function(x) exp(x))
	Y<-Y/rowSums(Y)
	return(Y)
}

#test
set.seed(123)
a<- runif(22,-1,1)

head( NNml(X, a , 4) )
head( NN(a, X) )

system.time(for(i in 1:1000){NNml(X, a , 4) })
system.time(for(i in 1:1000){ NN(a, X) })

mllogL<-function(x,y,eta,m){
	Y<-NNml(x,eta,m)
	return( sum( (1-y)*log(Y[,1]) + ( y)*log(Y[,2])  ) )	}

mllogL(X,nn[,3],a,4)
logL(a, X, nn[,3] )

gradL(a,X,nn[,3],4 )
grad_logL(a,X,nn[,3])


yy<-nn[,3]

logL(a, X,yy)
mllogL(X,nn[,3],a,4)


ETA <-a 
ETA[i]<-a[i]+(10^-6)
YYp<-NNml(X, ETA, 4)
sum( (1-yy)*log(YYp[,1]) + ( yy)*log(YYp[,2])  )












