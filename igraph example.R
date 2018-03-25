# loading karate dataset
library(igraph)
 rm(list = ls())

setwd("G:\\math\\504")
d <- read.table("karateclub.txt")
g=graph.adjacency(as.matrix(d),mode="undirected",weighted=NULL,diag=FALSE) 
plot.igraph(g)


A <- read.table("karateclub.txt")

dim(A)

k<- (rowSums(A)) 
sum(A)/2
k


 
m<-1/2 * sum(rowSums(A)) 
m
 
B<-matrix(NA,dim(A)[1],dim(A)[1])

for( i in 1:nrow(B)){
for( j in 1:ncol(B)){

	B[i,j]<-A[i,j] - 	( ( sum(A[,i])*sum(A[,j]) )  / (2*m) ) }}
B
eigen(B)
u1<-eigen(B)$vector[,1]
s<-ifelse(u1 > 0 , 1,-1)

Q<-(1/(4*m))  * (t(as.matrix(s)) %*% B %*% as.matrix(s))

Q
s

g=graph.adjacency(as.matrix(A),mode="undirected",weighted=NULL,diag=F) 
plot.igraph(g)
V(g)$name<-1:(dim(A)[1])
V(g)$color<-ifelse(s == 1, "blue", "red")
plot.igraph(g)



# manual eigenvectors

Pi<-function(M){
	x<-matrix(1,ncol(M),ncol(M))
	v<-matrix(NA,ncol(M),ncol(M))
	rq<-rep(NA,ncol(M))

	repeat{
		y<-x
		for( i in 1:ncol(M) ){	v[,i]<-M%*%x[,i] }
		x<-qr.Q(qr(v))	
			if (sum(abs(x[,1]-y[,1])) < 10^-6) { break }}
	for( i in 1:ncol(M) ){rq[i] <-
		t(x[,i]) %*% M %*% x[,i]/sum(x[,i]*x[,i]) }
	mylist<-list(x,round(rq,10))
	return(mylist) 
}

Pi(B)[[2]];eigen(B)$values
Pi(B)[[1]][,2]
eigen(B)$vector[,1]

