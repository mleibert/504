rm(list = ls())
 setwd("G:\\math\\504")
options(scipen=999)
 
NASA<-read.table("o_ring_data.txt",header=T)


nn<-read.table("nn.txt",header=T)

plot(nn$x1,nn$y ,col="red",pch=0)
points(nn$x2,nn$y, col="blue",pch=1)

max(nn$y)

X1.fit<-glm(y~x1+x2 ,data=nn,family=binomial())
X2.fit<-glm(y~x2 ,data=nn,family=binomial())

A<-as.numeric(c(coef(X1.fit),	coef(X2.fit)))
 
Z1<-as.vector( predict(X1.fit, type="response") )
Z2<-as.vector( predict(X2.fit, type="response") )

Z1.fit<-glm(nn$y~Z1 ,  family=binomial())
Z2.fit<-glm(nn$y~Z2 , family=binomial())
T1<-as.vector( predict(Z1.fit, type="response") )
T2<-as.vector( predict(Z2.fit, type="response") )

Y1<-exp(T1) / ( exp(T1) + exp(T2) )
Y2<-exp(T2) / ( exp(T1) + exp(T2) )



X<-as.matrix(nn[,-3])
X

sigmoid<-function(a,X){ 
	(1+exp(-a[1]-	apply(X,1, function(x)  t(a[-1])%*%x )	))^(-1)}

a<-c( 0.2447 ,      0.0186     ,  0.2031)
 all( round(sigmoid(a,X) -  predict(X1.fit, type="response"),4)==0)


#
newtonraph(c(0,0),nn$x1,nn$y)

newtonraph<-function(a,x,y){
	i=0
	repeat{
		alphaold<-a

		G1<-sum( ((1)/(exp(a[1]+a[2]*x)+1)) + y -1  ) 
		G2<-sum( ((x)/(exp(a[1]+a[2]*x)+1)) + x * (y-1) ) 
		G<-c(G1,G2)
		
		H11= -sum( ( exp(-a[1]-a[2]*x ) ) /
			( ( 1+exp(-a[1]-a[2]*x )  )^2	) )
		H12= -sum( x*( exp(-a[1]-a[2]*x ) )/
			(( 1+ exp(-a[1]-a[2]*x )  )^2	) )
		H22= -sum( x^2*( exp(-a[1]-a[2]*x ) )/
			( (1+ exp(-a[1]-a[2]*x )  )^2	) )
		H <- matrix(c(H11, H12, H12, H22), 2, 2)

 		a <- a - solve(H)%*%G

		i=i+1
		dat<-data.frame(a[1],a[2], i)
		names(dat)<-c("a0","a1","iterations")
		
		if (  (abs(a[1] - alphaold[1]) < 10^-6) &
			(abs(a[2] - alphaold[2]) < 10^-6)
			){ print(dat); break} else {next}}

}


