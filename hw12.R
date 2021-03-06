rm(list = ls())
 setwd("G:\\math\\504")
options(scipen=999)
 require("ggplot2")

nn<-read.table("nn.txt",header=T)	
head(nn)

p <- ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y)))
p 

X<-as.matrix(nn[,-3])
X

sigmoid<-function(a,X){ 
	(1+exp(-a[1]-	apply(X,1, function(x)  t(a[-1])%*%x )	))^(-1)}

#Generate some temp alpha parameters
l.fit<-glm(y~x1+x2 ,data=nn,family=binomial())
p.fit<-glm(y~x1+x2 ,data=nn,family=binomial(link=probit))
ll.fit<-glm(y~x2+x1 ,data=nn,family=binomial(link=cloglog))
cll.fit<-glm(y~x1+x2 ,data=nn,family=binomial(link=cauchit))

A<-cbind(coef(l.fit),coef(p.fit),coef(ll.fit),coef(cll.fit))


#all( round(sigmoid(A[1],X) -  predict(l.fit, type="response"),15)==0)

Z<-apply(A,2, function(x) sigmoid(x,X))





DATz<-as.data.frame(cbind(Z,nn$y))
lz.fit<-glm(V5~. ,data=DATz,family=binomial())
pz.fit<-glm(V5~. ,data=DATz,family=binomial(link=cloglog))

B<-cbind(coef(lz.fit),coef(pz.fit) )

T<-apply(B,2, function(x) sigmoid(x,Z))
Y<- apply(T,2, function(x) exp(x))
Y<-Y/rowSums(Y)




#https://www.youtube.com/watch?v=NjWKeL25ows

eta<-runif(22,-1, 1)

NN(X,c(as.vector(A),as.vector(B)),4)



sum( (1-y)*log(Y[,1]) + ( y)*log(Y[,2])  )


gradF<-funtion(eta,)
for( i in 1:22){

	ETA<-eta
	ETA[i]<-eta[i]+(10^-6)

	Yp<-NN(x,ETA,4)
	gradF[i]<-(sum( (1-y)*log(Yp[,1]) + ( y)*log(Yp[,2])  ) - 
		sum( (1-y)*log(Y[,1]) + ( y)*log(Y[,2])  )  ) / (10^-6)
}



#############

bank<-read.table("G:\\math\\661\\bank_loan.txt",header=T)
data.frame(colnames(bank),1:ncol(bank))
bank<-bank[,c(3,4,6,7,9)]

bank<-bank[1:700,]
set.seed(1)
a<-sample(1:700,600)

banktest<-bank[-a,]
bank<-bank[a,]

fit.lasso = glm(default ~ employ+address+debtinc+creddebt,
	family="binomial", data=past)
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


