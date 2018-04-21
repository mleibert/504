rm(list = ls())
 setwd("G:\\math\\504")
options(scipen=999)
 
NASA<-read.table("o_ring_data.txt",header=T)


nn<-read.table("nn.txt",header=T)

#plot(nn$x1,nn$y ,col="red",pch=0)
#points(nn$x2,nn$y, col="blue",pch=1)

X<-as.matrix(nn[,-3])
X

#same as predict function
sigmoid<-function(a,X){ 
	(1+exp(-a[1]- apply(X,1, function(x)  t(a[-1])%*%x )	))^(-1)}

#Generate some temp alpha parameters
l.fit<-glm(y~x1+x2 ,data=nn,family=binomial())
p.fit<-glm(y~x1+x2 ,data=nn,family=binomial(link=probit))
ll.fit<-glm(y~x2+x1 ,data=nn,family=binomial(link=cloglog))
cll.fit<-glm(y~x1+x2 ,data=nn,family=binomial(link=cauchit))

#Capturing the A's for eta^0 for the gradient ascent
A<-cbind(coef(l.fit),coef(p.fit),coef(ll.fit),coef(cll.fit))


#all( round(sigmoid(A[2],X) -  predict(p.fit, type="response"),15)==0)

#Z_j = sigma(x'A) [ x transpose A ] j=1,2,3,4
Z<-apply(A ,2, function(q) sigmoid(q,X))


#Generate some temp beta parameters
DATz<-as.data.frame(cbind(Z,nn$y))
lz.fit<-glm(V5~. ,data=DATz,family=binomial())
pz.fit<-glm(V5~. ,data=DATz,family=binomial(link=cloglog))


#Capturing the B's for eta^0 for the gradient ascent
B<-cbind(coef(lz.fit),coef(pz.fit) )

#all( round(sigmoid(B[1],X) -  predict(lz.fit, type="response"),15)==0)

T<-apply(B,2, function(x) sigmoid(x,Z))

Y<- apply(T,2, function(x) exp(x))
Y<-Y/rowSums(Y)




#https://www.youtube.com/watch?v=NjWKeL25ows




Z1<-as.vector( predict(X1.fit, type="response") )
Z2<-as.vector( predict(X2.fit, type="response") )

Z1.fit<-glm(nn$y~Z1 ,  family=binomial())
Z2.fit<-glm(nn$y~Z2 , family=binomial())
T1<-as.vector( predict(Z1.fit, type="response") )
T2<-as.vector( predict(Z2.fit, type="response") )

Y1<-exp(T1) / ( exp(T1) + exp(T2) )
Y2<-exp(T2) / ( exp(T1) + exp(T2) )

 


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


