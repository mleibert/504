rm(list = ls())

library(expm)
 

mat<-matrix(0,5,5)

for( i in 1:5){
	  y<-sample(1:5,sample(2:5,1))
	  x<-sample(1:10,length(y)) 
	  x<-x/sum(x)
	  mat[i,y]<-x	
}

mat

rowSums(mat)
 
 Re(eigen(t(mat))$vectors[,1]) / Re( sum(eigen(t(mat))$vectors[,1])  )


mat%^%100
p=.15
m=5
matB<-matrix(1/m,m,m)
rowSums( (1-p) * mat ) 

rowSums( ( p) * matB) 

matA<-(1-p) * mat + ( p) * matB
rowSums( matA)
xx<-1:5
system.time(
repeat{
	y<-xx
	xx<- (t(mat)%*%xx)  / norm((t(mat)%*%xx))
	xx

 	if( norm(xx-y) < 10^-16 ) {break} }
)


x<-sample(1:10,m,replace=T)
repeat{
	y<-x
	x<- ( (1-p) * t(mat) %*%x ) + ( rep( (1/m)* p * sum(x),m) ) / 
		norm(( (1-p) * t(mat) %*%x ) + ( rep( (1/m)* p * sum(x),m) ))
 	if( norm(x-y) < 10^-14 ) {break} }

x

  G<-( (1-p) * mat  + ( p) * matrix(1/m,m,m) ) 
 
 Re(eigen(t(G))$vectors[,1]) / Re( sum(eigen(t(G))$vectors[,1])  )

######################################################################

setwd("g:\\math\\504")
edge<-read.table("edges.txt")
node<-read.table("nodes.txt")
options(scipen=999)

G<-matrix(0,max(edge),max(edge))
n=max(edge)
 


for( i in 1:max(edge) ) {

	if( length( which(edge[,1]  == i)) == 0 ){ G[i,i]<-1;next}
	G[i,edge[ which(edge[,1]  == i) ,2]]<-1 }
	

 G<- apply(G, 2, "/",  rowSums(G)  )
#Gt[,666]


B<-matrix(1/n,n,n)
p=.15

######## 

A<-(1-p) * G + ( p) * B

 all(round(rowSums(A),15) == 1)
 

x<-sample(1:10,max(edge),replace=T)
system.time(	
repeat{
	y<-x
	x<- (t(A)%*%x)  / norm((t(A)%*%x))
	if( norm(x-y) < 10^-6 ) {break} }
)

######## 211 seconds

x<-sample(1:10,max(edge),replace=T)
system.time(	
repeat{
	y<-x
	x<- ( (1-p) * t(G) %*%x ) + ( p  * B %*% x ) / 
		norm(( (1-p) * t(G) %*%x ) + ( p  * B %*% x ))
	if( norm(x-y) < 10^-6 ) {break} }
)

node[,1]<-x

head( node[with(node, order(V1,decreasing = T)), ] , 20)

#Re(eigen(t(mat))$vectors[,1]) / Re( sum(eigen(t(mat))$vectors[,1])  )
 


###########	177 seconds
library(Matrix)

nodes<-read.table("nodes.txt")
rm(B)

x<-sample(1:10,max(edge),replace=T)
system.time(	
repeat{
	y<-x
	x<- ( (1-p) * t(G) %*%x ) + ( rep( (1/n)* p * sum(x),n) ) / 
		norm(( (1-p) * t(G) %*%x ) + ( rep( (1/n)* p * sum(x),n) ))
	if( norm(x-y) < 10^-6 ) {break} }
)

 

nodes$P<-x
head( nodes[with(nodes, order(P,decreasing = T)), ] , 10)
head( node[with(node, order(P,decreasing = T)), ] , 10)


###

edge<-read.table("edges.txt")
nodez<-read.table("nodes.txt")

dat<-data.frame(setdiff(edge$V2,edge$V1), setdiff(edge$V2,edge$V1))
names(dat)<-names(edge)
edge<-rbind(edge,dat)
rm(dat)

dat<-data.frame(setdiff(edge$V1,edge$V2), setdiff(edge$V1,edge$V2))
names(dat)<-names(edge)
edge<-rbind(edge,dat)
rm(dat)


setdiff(edge$V2,edge$V1)
setdiff(edge$V1,edge$V2)
edge$V3<-1
edge[nrow(edge),3]<-edge[(nrow(edge)-1),3]<-0
tail(edge)

A <- sparseMatrix(i = edge$V1, j = edge$V2, x = edge$V3)
A <- A/rowSums(A)
n=dim(A)[1]
p=.15
 

 
x<-sample(1:10,n,replace=T)
system.time(	
repeat{
	y<-x
	x<- as.matrix( (1-p) * t(A) %*%x ) + ( rep( (1/n)* p * sum(x),n) ) / 
		norm(( (1-p) * t(A) %*%x ) + ( rep( (1/n)* p * sum(x),n) ))
	if( norm(x-y) < 10^-6 ) {break} }
)


nodez$P<-x
head( nodez[with(nodez, order(P,decreasing = T)), ] , 10)
head( nodes[with(nodes, order(P,decreasing = T)), ] , 10)
head( node[with(node, order(P,decreasing = T)), ] , 10)


########################################################################
########################################################################

setwd("g:\\math\\504")
edge<-read.table("web-NotreDame.txt")

length ( unique(edge$V1) );length ( unique(edge$V2) )

 
head(edge) 

dat<-data.frame(setdiff(edge$V2,edge$V1), setdiff(edge$V2,edge$V1))
names(dat)<-names(edge)
edge<-rbind(edge,dat)
rm(dat)


setdiff(edge$V2,edge$V1)
setdiff(edge$V1,edge$V2)


edge$V1<-edge$V1+1
edge$V2<-edge$V2+1

head(edge)
edge$V3<-1



library(Matrix)
A <- sparseMatrix(i = edge$V1, j = edge$V2, x = edge$V3)
A <- A/rowSums(A)
n=dim(A)[1]
p=.15
 

x<-sample(1:10,n,replace=T)
system.time(	
repeat{
	y<-x
	x<- as.matrix( (1-p) * t(A) %*%x ) + ( rep( (1/n)* p * sum(x),n) ) / 
		norm(( (1-p) * t(A) %*%x ) + ( rep( (1/n)* p * sum(x),n) ))
	if( norm(x-y) < 10^-6 ) {break} }
)

dat<- ( data.frame(x ,  0:(length(x)-1)) ) 
names(dat)<-c("p","page")
tail(dat)
 head( dat[with(dat, order(p,decreasing = T)), ] , 10)

