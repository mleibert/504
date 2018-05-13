rm(list = ls())
 setwd("G:\\math\\504")
options(scipen=999)
 #require("ggplot2")

bones<-read.table("BoneMassDataF.txt",header=T)
bones<-bones[which(bones[,3] == "female"),]
 
# x=bones[,2];a=min(bones[,2]);b=max(bones[,2]);n=10000

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

Fapprox(10000,5,0,bones[,2])


sum( Fx(0,a+(i+1)*h)*Fx(5,a+(i+1)*h)*h )



for( J in 0:5){
for( K in 0:5){ 	mat[J+1,K+1]<-Fapprox(10000,J,K,bones[,2]) }}
mat

Norm <- function(w){  sqrt(sum(w^2))}
diag(mat)[2]


Gapprox<-function(n,j,k,x){
	
	coef<-(1/sqrt( diag(mat) )) 
	Fx<-function(l,x){ if(l != 0) {
		return( coef[l+1]* cos( (2*pi*l*(x-9.4 ) ) /(25.55-9.4 )) )
		} else  {return( coef[l+1]* rep(1,length(x) ))} }

	a=min(x);b=max(x)
	
	h=(b-a)/n;i=0:(n-1)
	return( 	sum( Fx(j,a+(i+1)*h)*Fx(k,a+(i+1)*h)*h )	)
}

Gapprox(10000,0,0,bones[,2])

matt<-matrix(NA,6,6)

for( J in 0:5){
for( K in 0:5){ 	matt[J+1,K+1]<-Gapprox(10000,J,K,bones[,2]) }}
diag(matt )

 
