
gradL<-function(eta,x,y,m ){
		
	y<-as.matrix(y)
	dimeta<-m*(dim(x)[2]+1)+2*(1+m)
	gradf<-rep(NA,dimeta)

	Y<-NNml(x,eta,m)
	ETA<-eta

	logl<-mllogL(x,y,eta,m)
	
	for( i in 1:dimeta){
		ETA <-eta
		ETA[i]<-eta[i]+(10^-6)
		#changed to -logL with +
		gradf[i]<- (mllogL(x,y,ETA,m) -	logl  ) / (10^-6)	}
	return(gradf)
}

gradL(a,X,nn[,3],4 ) 

Hfd<-function(eta,x,y,m ,d){
		
	y<-as.matrix(y)
	dimeta<-m*(dim(x)[2]+1)+2*(1+m)
	Hf<-rep(NA,dimeta)
	epi<-10^-6

	Y<-NNml(x,eta,m)
	ned<-eta+(epi*d)
	

	logl<-mllogL(x,y,ned,m)
	
	for( i in 1:dimeta){
		NED<-ned
		NED[i]<-ned[i]+(epi)
		#changed to -logL with +
		Hf[i]<- (mllogL(x,y,NED,m) -	logl  ) / (epi)	}

	d<-(Hf[i]-gradL(eta,x,y,m))/epi
	return(d)
}


Hfd(a,X,nn[,3],4,rep(.1,22))

AA<-matrix(c(2,-1,0,-1,2,-1,0,-1,2),3,3,byrow=T)
bb<-matrix(c(2,2,6),3,1)
solve(AA,bb)
AA<-matrix(c(2,2,2,5 ),2,2,byrow=T)
bb<-matrix(c(6,3),2,1)
solve(AA,bb)

function(A,B,x)

A<-AA
b<-bb
x<-rep(.2,2)
 d<- b-(A%*%x )
 r<- b-A%*%x 
solve(A,b)
 
eigen(A)

x<-rep(0.2,3)
 d<- r<- b-(A%*%x )

repeat{
Alpha<-as.numeric(  (t(r)%*%r)/(t(d)%*%A%*%d) )
xnext<-x+Alpha*d
rnext<-r-Alpha*A%*%d
if( all(as.numeric( round(rnext,10) ) == 0) == T ){print(xnext)}
if( all(as.numeric( round(rnext,10) ) == 0) == T ){break}
Beta<- as.numeric((t(rnext)%*%rnext)/(t(r)%*%r))
dnext<-rnext+Beta*d
d<-dnext;r<-rnext;x<-xnext 
}
x