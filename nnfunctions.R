
NN<-function(x,eta,m){
	
	X<-as.matrix(x)

	Alpha<-matrix(eta[1:prod( c(dim(X)[2] + 1, m ) )], dim(X)[2] + 1 , m)
	Z<-apply(Alpha, 2, function(w) 1/(1+exp(- w[1] -   X %*% w[-1]  )) ) 

	Beta<-matrix( eta[(prod(dim(Alpha))+1):length(eta) ], dim(Z)[2]+1, 2)
	 
	#TT<-apply(Beta,2, function(v) sigmoid(v,Z))
	TT<-apply(Beta, 2, function(w) 1/(1+exp(- w[1] - Z %*% w[-1]  )) ) 
	Y<- apply(TT,2, function(x) exp(x))
	Y<-Y/rowSums(Y)
	return(Y)
}

logL<-function(x,y,eta,m){
	Y<-NN(x,eta,m)
	return(- sum( (1-y)*log(Y[,1]) + ( y)*log(Y[,2])  ) )	}

gradL<-function(x,y,eta,m ){
		
	y<-as.matrix(y)
	dimeta<-m*(dim(x)[2]+1)+2*(1+m)
	gradf<-rep(NA,dimeta)

	Y<-NN(x,eta,m)
	ETA<-eta

	logl<-logL(x,y,eta,m)
	for( i in 1:dimeta){
		ETA <-eta
		ETA[i]<-eta[i]+(10^-6)
			
		gradf[i]<- ( logL(x,y,ETA,m) -	logl  ) / (10^-6)	}
	return(gradf)
}


Hfd<-function(x,y,eta,m,d,epi=10^-3 ){
	(gradL(x,y,eta+d*epi,m)-gradL(x,y,eta,m))/(epi)}
