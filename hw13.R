rm(list = ls())
setwd("G:\\math\\504")
options(scipen=999)
 Norm <- function(w){  sqrt(sum(w^2))}
require(ggplot2)

dat<-read.table("user-shows.txt")
shows<-read.table("shows.txt")
alex<-read.table("alex.txt");colnames(alex)<-as.vector(shows$V1)

A<-matrix(c(2,4,1,3,0,0,0,0),4,2,byrow=T)
A

X<-as.matrix(dat)
dim(X)
dat<-as.data.frame(dat)
colnames(dat)<-as.vector(shows$V1)
 
#SVD functions
#sdat<-svd(as.matrix(dat))
#plot(sdat[[1]])
#dim(sdat[[2]])
#dim(sdat[[3]])
#sum(sdat[[1]])-cumsum(sdat[[1]])
 
V<-eigen(t(X)%*%X)$vectors
U<-diag(dim(X)[1])
S<-matrix(0,dim(X)[1],dim(X)[2])
for( i in 1:dim(V)[1]){ a<- X%*%V[,i]; U[,i] <- a/Norm(a); S[i,i]<-Norm(a)}  
rm(a)

plot(diag(S),xlim=c(0,20))

#same result as SVD function
sum(diag(S))-cumsum(diag(S))
head(diag(S));head(sdat[[1]])

sdat[[1]][1]

Ahat<-S[1,1] * U[,1] %*% t(V[,1]) + S[2,2] * U[,2] %*% t(V[,2])


U<-as.data.frame(sdat$u[,1:2]);V<-as.data.frame(sdat$v[,1:2] )
UU<-data.frame(apply(U , 1, function(z) Norm(z-as.numeric(U[ 500,]) )^2),
	1:length(UU))
UU<-data.frame(apply(U , 1, function(z) Norm(z-as.numeric(U[ 500,]) )^2),1:nrow(U))

which(dat[500,] == 1)

VV<-data.frame(apply(V , 1, function(z) Norm(z-as.numeric(V[ 500,]) )^2),
	1:length(UU))


buddies<-order(UU[,1])[ 2:31]
 
ggplot(U,aes(V1,V2)) + geom_point() +  
  geom_point(data=U[500,], colour="red")+  
 geom_point(data=U[buddies,], colour="green")


ggplot(U,aes(V1,V2)) + geom_point() +  
  geom_point(data=U[500,], colour="red")+  
  xlim(-0.008173631-.001, -0.008173631+.001)  +
	 ylim(-0.0008980471-.001, -0.0008980471+.001) +
 geom_point(data=U[buddies,], colour="green")


recommend<-dat[buddies,]
recommend<-rbind(recomend,colSums(recommend))
recommend[ 10,which( recommend[10, ] > 5 )]
 

dat[500 , which(dat[500,] == 1)] 


ggplot(V,aes(V1,V2)) + geom_point() +  
  geom_point(data=V[ which(dat[500,] == 1),], colour="red")  +
	 geom_point(data=V[ Mr,], colour="green") 



 V[ which(dat[500,] == 1),][1,]

NO<-which(dat[500,] == 1)
Mr<-rep(NA,length(which(dat[500,] == 1)) )
system.time(
for(i in 1:length(Mr) ){
	dattt<- (data.frame(apply(V[-NO,] , 1, function(z) Norm(z-
		V[which(dat[500,] == 1),][i,]  )^2),1:(nrow(V)-length(NO))) )
	Mr[i]<-order(dattt[  ,1])[1] }
)

recon<-dat[,Mr]
which( colnames(recon) %in%
	colnames( recommend[ 10,which( recommend[10, ] > 15 )] )
 )

colnames( recommend[ 10,c(16,20,56,88)] )

FOX 28 News at 10pm
which(  colnames( recommend[ 10,which( recommend[10, ] > 5 )] )%in%
	 colnames(recon))

colnames( recommend[ 10,c(3,9,16,20)] )


######################################################
######################################################
######################################################
######################################################

x<-1:5
y<-as.matrix( sin(x) )

daf<-data.frame(x,y)

MM <- function(x) { 
	nx <- length(x)
	m <- cbind(rep(1, nx), x, x^2, x^3, x^4, x^5, x^6 )
	colnames(m )<-NULL
	return(m)	}

B<-MM(x)

Alpha0<-solve(t(B) %*% B + 0 + diag(max(dim(B))) ) %*% t(B) %*% y
Alpha1<-solve(t(B) %*% B + 1 + diag(max(dim(B))) ) %*% t(B) %*% y
Alpha10<-solve(t(B) %*% B + 10 + diag(max(dim(B))) ) %*% t(B) %*% y

svd(B)


dim(B)
V1<-eigen(t(B)%*%B)$vectors
U1<-diag(dim(B)[1])
S1<-matrix(0,dim(B)[1],dim(B)[2])
for( i in 1:min(c(dim(V1),dim(U1))) ){ 
	a<-B%*%V1[,i]; U1[,i]<-a/Norm(a); S1[i,i]<-1/Norm(a)} ;rm(a)
 

PS<- V1 %*% t(S1) %*% t(U1) %*% y 


x_grid <- seq(min(x), max(x), .001)
B_grid <- model_matrix(x_grid)
y_gridps <-B_grid  %*% PS
y_grid0 <-B_grid  %*% Alpha0
y_grid1 <-B_grid  %*% Alpha1
y_grid10 <-B_grid  %*% Alpha10


 

par(mfrow=c(2,2))
plot(x,y , ylim = c(-1.5,1.5) , main="Pseudoinverse" )
lines(x_grid, y_gridps , col="mediumorchid1", lwd=2)
plot(x,y, ylim = c(-1.5,1.5),main=expression(paste("Penalized, ", rho, "=0")))
lines(x_grid, y_grid0, col="firebrick1", lwd=2)
plot(x,y,ylim = c(-1.5,1.5),main=expression(paste("Penalized, ", rho, "=1")))
lines(x_grid, y_grid1, col="springgreen", lwd=2)
plot(x,y,ylim = c(-1.5,1.5),main=expression(paste("Penalized, ", rho, "=10")))
lines(x_grid, y_grid10, col="steelblue1", lwd=2)


#For the penalized methods we can see that the $rho$ is giving a better fit 
#for the smaller $rho$'s. We do note that all the penalized methods are just
#approximations rather than solutions. The pseudoinverse is actually a 
#solution for $B\alpha=y$ and we see it going precisely through each point.


