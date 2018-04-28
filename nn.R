rm(list = ls())
 setwd("G:\\math\\504")
nn<-read.table("nn.txt",header=T)

#plot(nn$x1,nn$y ,col="red",pch=0)
#points(nn$x2,nn$y, col="blue",pch=1)

X<-as.matrix(nn[,-3])
X


	sigmoid<-function(a,w){	(1+exp(-a[1]-	
		apply(w,1, function(w)  t(a[-1])%*%w )	))^(-1)}


NN<-function(x,eta,m){
	
	sigmoid<-function(a,w){	(1+exp(-a[1]-	
		apply(w,1, function(w)  t(a[-1])%*%w )	))^(-1)}

	X<-as.matrix(x)

	Alpha<-matrix(eta[1:prod( c(dim(X)[2] + 1, m ) )], dim(X)[2] + 1 , m)
	Z<-apply(Alpha,2, function(v) sigmoid(v,X))

	Beta<-matrix( eta[(prod(dim(Alpha))+1):length(eta) ], dim(Z)[2]+1, 2)
	 
	T<-apply(Beta,2, function(v) sigmoid(v,Z))
	Y<- apply(T,2, function(x) exp(x))
	Y<-Y/rowSums(Y)
	return(Y)
}
 

gradL<-function(eta,x,y,m ){
		
	y<-as.matrix(y)
	dimeta<-m*(dim(x)[2]+1)+2*(1+m)
	gradf<-rep(NA,dimeta)

	Y<-NN(x,eta,m)
	ETA<-eta

	logl<-sum( (1-y)*log(Y[,1]) + ( y)*log(Y[,2])  )
	for( i in 1:dimeta){
		
		ETA[i]<-eta[i]+(10^-6)
			
		Yp<-NN(x,ETA,m)
		
		gradf[i]<-(sum( (1-y)*log(Yp[,1]) + ( y)*log(Yp[,2])  ) - 
			logl  ) / (10^-6)	}
	return(gradf)
}

logL<-function(x,y,eta,m){
	Y<-NN(x,eta,m)
	return( sum( (1-y)*log(Y[,1]) + ( y)*log(Y[,2])  ) )	}

Norm <- function(w){  sqrt(sum(w^2))}

PlogL<-function(x,y,eta,m,rho){
	X<-as.matrix(x)
	Alpha<-matrix(eta[1:prod( c(dim(X)[2] + 1, m ) )], dim(X)[2] + 1 , m)
 	Beta<-matrix(eta[(prod( c(dim(X)[2] + 1, m ) )+1):length(eta)],
		m+1,2)
	Alpha<-as.vector(t(Alpha[-1 ,])) ;Beta<-as.vector(t(Beta[-1 ,]))

	Y<-NN(x,eta,m)
	return( sum( (1-y)*log(Y[,1]) + ( y)*log(Y[,2])  ) -
		rho*(sum(Norm(Alpha)^2)+sum(Norm(Beta)^2) )
	)	}

logL(X,nn[,3],runif(22,-11, 11) ,4)
#-1346
######################################################################
######################################################################
######################################################################
######################################################################



 set.seed(1);aaa=runif(22,-1, 1) 

 mylist<-list()
 mylist[[1]]<-aaa



system.time( for(i in j:(j+5300)){ 

	aaa<- aaa +  .5 *  gradL(aaa, X , nn[,3] , 4 )
			 mylist[[i+1]]<-aaa  } )
aaa

j<-i
 mylist[[1]]
 mylist[[900]]
 mylist[[i]]

###

NN(X,aaa,4)


 set.seed(1);Alpha=runif(22,-1, 1) 
 set.seed(4);ALPHA=runif(22,-1, 1) 

j=1;current_grad <- gradL(Alpha , nn[,-3] , nn[,3] , 4 ) 
system.time(
 for(i in j:(4000)) {
    #iter <- iter + 1
	
	d <- current_grad/Norm(current_grad)	
	
	s <- 1 
	 current_logL <- logL(X,nn[,3],Alpha,4)
	 current_logL <- PlogL(X,nn[,3],Alpha,4,100)
    
	while(logL(X,nn[,3],Alpha+s*d,4)  < current_logL)
      	 s <- s/2 

    # update
    Alpha <- Alpha + s*d
    current_grad <-  gradL(Alpha , X , nn[,3] , 4 ) } )


j=i
logL(X,nn[,3],Alpha* 1.5 ,4)	 


NN(nn[,-3],Alpha ,4)
nnn<-cbind(nn,	NN(nn[,-3],Alpha ,4)	)

function( iteratez,seondz){

	5*20


Alpha <-c(-3.7748365, 2.1178734, 0.3139542, -5.5415006, -.5958018, 
	5.8168459, 11.0561352, 0.2074085, 14.0207743, 25.2868297, 16.0428078, 
	3.8300725, 15.9792114, 23.4815794, 23.2210775, -13.5097426, -12.2073593, 
	-18.6510642, -27.0546378, -27.4979835, 15.2350215, 14.7081561)
 blerg<-read.csv("G:/math/504/a01.csv")
Alpha<-blerg[,ncol(blerg)];Alpha

logL(nn[,-3],nn[,3],Alpha,4)

x1<-matrix(4*runif(2000 )-2, 1000,2)
x1<-nn[,-3]
test<-as.data.frame((NN(x1,Alpha ,4)))
test<-cbind(test,x1)
test$y3<- ifelse( test[,1]>.3 , 0,1)
test$y3<- ifelse( test[,1]>.5 , 0,1)

names(test)[3:4]<-c("x1","x2")
 
p <- ggplot(test, aes(x1, x2)) + geom_point(aes(colour = factor(y3)))  
 p 


 
realp<-ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y))) 

p <- ggplot(test, aes(x1, x2)) + geom_point(aes(colour = factor(y3)))  
 p 


dev.new()

p <- ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y))) +   
  theme(legend.position="bottom");p 

p <- ggplot(test, aes(x1, x2)) + geom_point(aes(colour = factor(clas))) +
	ylim(-2.5, 2.5) + xlim( -2.5,2.5 )
 p 

