rm(list = ls())
 setwd("G:\\math\\504")
#options(scipen=999)
 #require("ggplot2")
bones<-read.table("BoneMassDataF.txt",header=T)
bones<-bones[which(bones[,3] == "female"),]
 
# x=bones[,2];a=min(bones[,2]);b=max(bones[,2]);n=10000;m=6

Fapprox<-function(n,j,k,x){
	
	Fx<-function(l,x){ if(l != 0) {
		return( cos( (2*pi*l*(x-9.4 ) ) /(25.55-9.4 )) )
		} else  {return(rep(1,length(x) ))} }
	#Gx<-function(s,t,x){ if(k != 0) {
	#	return( cos( (2*pi*s*(x-9.4 ) ) /(25.55-9.4 )) *
	#	cos( (2*pi*t*(x-9.4 ) ) /(25.55-9.4 ))	)
	#	} else  {return(rep(1,length(x) ))} }

	a=min(x);b=max(x)
	h=(b-a)/n;i=0:(n-1)
	return( 	sum( Fx(j,a+(i+1)*h)*Fx(k,a+(i+1)*h)*h )	)
	#QQ<-( sum( Gx(j,k,a+(i+1)*h)*h )	)
	#newList <- list("one" = qq, "two" = QQ);return(newList)
}

mat<-matrix(NA,6,6)


for( J in 0:5){ for(K in 0:5){mat[J+1,K+1]<-Fapprox(10000,J,K,bones[,2]) }}
mat



Norm <- function(w){  sqrt(sum(w^2))}
 
Gapprox<-function(n,j,k,x){
	
	coeff<-(1/sqrt( diag(mat) )) 
	Fx<-function(l,x){ if(l != 0) {
		return( coeff[l+1]* cos( (2*pi*l*(x-9.4 ) ) /(25.55-9.4 )) )
		} else  {return( coeff[l+1]* rep(1,length(x) ))} }

	a=min(x);b=max(x)
	
	h=(b-a)/n;i=0:(n-1)
	return( 	sum( Fx(j,a+(i+1)*h)*Fx(k,a+(i+1)*h)*h )	)
}

Gapprox(10000,0,0,bones[,2])

matt<-matrix(NA,6,6)
for( J in 0:5){for( K in 0:5){ matt[J+1,K+1]<-Gapprox(10000,J,K,bones[,2]) }}
diag(matt )

#### b



Fx<-function(w,x){   
	mat<-matrix(NA,6,6)
	#need this matrix for the normalizing Coefficients
	for( J in 0:5){ for(K in 0:5){
		mat[J+1,K+1]<-Fapprox(10000,J,K,x) }}
	
	ncoef<-(1/sqrt( diag(mat) )) 
	if(w == 0 ) {return(   rep(1,length(x) ))  } else {
	return( ncoef[w+1]* cos( (2*pi*w*(x-9.4 ) ) /(25.55-9.4 )) )
		} }
 

 

model_matrix <- function(x) { 
	nx <- length(x)
	m <- cbind(Fx(0,x),Fx(1,x), Fx(2,x), Fx(3,x), Fx(4,x), Fx(5,x) )
	colnames(m )<-NULL
	return(m)	}

B<- model_matrix(bones$age,5)
Alpha <- solve(t(B) %*% B, t(B) %*% as.matrix(bones[,4]));Alpha

x_grid <- seq(min(bones$age), max(bones$age), .01)
B_grid <- model_matrix(x_grid,5)
y_grid <- B_grid %*% Alpha
 
par(mar=c(4.1,4.1,1,1))
plot(bones$age, bones[,4], ylim=c(min(bones[,4])-.02,max(bones[,4]) + .02) )
lines(x_grid, y_grid, col="red", lwd=2)   












