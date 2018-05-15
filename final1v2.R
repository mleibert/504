Fapprox<-function(m,n=10000,x){

MATT<-matrix(0:(m-1),m,1)

Gx<-function(v,x){ if(v != 0) {
	return( cos( (2*pi*v*(x-9.4 ) ) /(25.55-9.4 )) )
	} else  {return(rep(1,length(x) ))} }

a=min(x);b=max(x)
h=(b-a)/n;i=0:(n-1)
W<-apply(MATT,1,function(w) Gx(w,a+(i+1)*h) )  

Mat<-matrix(NA,m,m)

 for( i in 1:m){
 Mat[,i] <-	 colSums( (  W[  ,i ]*W[  ,1:m]  ) ) * h 
} 

return(Mat)
}

 mattt<-Fapprox(6,n=10000, bones$age)
#mat<-Fapprox(1001,n=10000, bones$age)
write.csv(mat,"fapprox.csv")
 write.csv(omega,"omega.csv")



 


Gapprox<-function(n=10000,j,k,x){
	
	coeff<-(1/sqrt( diag(mat) )) 
	Fx<-function(l,x){ if(l != 0) {
		return( coeff[l+1]* cos( (2*pi*l*(x-9.4 ) ) /(25.55-9.4 )) )
		} else  {return( coeff[l+1]* rep(1,length(x) ))} }

	a=min(x);b=max(x)
	
	h=(b-a)/n;i=0:(n-1)
	return( 	sum( Fx(j,a+(i+1)*h)*Fx(k,a+(i+1)*h)*h )	)
}

 





for( J in 0:5){for( K in 0:5){ matt[J+1,K+1]<-Gapprox(10000,J,K,bones[,2]) }}





############

dBmat<-function(m,n=10000,x){

MATT<-matrix(0:(m-1),m,1)

dFx<-function(v,x){ if(v != 0) {
	return( -((4*pi^2*v^2)/(16.15^2))* cos( (2*pi*v*(x-9.4 ))/16.15)
	)	} else  {return(rep(1,length(x) ))} }

a=min(x);b=max(x)
h=(b-a)/n;i=0:(n-1)
W<-apply(MATT,1,function(w) dFx(w,a+(i+1)*h) )  

#Mat<-matrix(NA,m,m)

#for( i in 1:m){
#Mat[,i] <-	 colSums( (  W[  ,i ]*W[  ,1:m]  ) ) * h 
} 

return(Mat)
}

dB<-dBmat(1000+1,n=10000, bones$age)

Omega<-dB

