

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


Hfd<-function(x,y,eta,m,d,epi=10^-2 ){
	(gradL(x,y,eta+d*epi,m)-gradL(x,y,eta,m))/(epi)}


 

AA<-matrix(c(2,-1,0,-1,2,-1,0,-1,2),3,3,byrow=T)
bb<-matrix(c(2,2,6),3,1)
solve(AA,bb)
eigen(AA)$values


CG<-function(A,b){
	x<-rep(0 ,dim(A)[1])
	d<- r<- b-(A%*%x)
	iter=0
	repeat{
		iter=iter+1
		Alpha<-as.numeric(  (t(r)%*%r)/(t(d)%*%A%*%d) )
		xnext<-x+Alpha*d
		rnext<-r-Alpha*A%*%d
		if( all(as.numeric( round(rnext,10) ) == 0) == T ){break}
		Beta<- as.numeric((t(rnext)%*%rnext)/(t(r)%*%r))
		dnext<-rnext+Beta*d
		d<-dnext;r<-rnext;x<-xnext 
	}
	return(list(x=xnext,iterations=iter)) }
  
CG(AA,bb)

dat<-mtcars[,c(1,3:6)]
dat<-var(dat)
eigen(dat)$values
bb<-rpois(dim(dat)[1],3)
solve(dat,bb)

CG(dat,bb)



