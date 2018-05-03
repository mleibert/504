rm(list = ls())
x<-read.csv("synthetic kmeans data.csv")
K<-2
	Norm <- function(w){  sqrt(sum(w^2))}

	N<-nrow(x);	p<-ncol(x)
	x<-cbind(x,matrix(NA,N,1));names(x)[ncol(x)]<-"Assignment"
	meanz<-matrix(NA,N,p)
	x[,ncol(x)]<-sample(1:K,N,replace=T);RAND<-c( 175, 192)
	mus<-matrix(NA,K,p); 
	for(i in 1:K){ mus[i,]<- as.numeric( x[RAND[i],1:p] ) }


gglist<-list()

for( L in 1:8){ 
	for( i in 1:K) { meanz[,i]<-apply(x[,1:p], 1, 
		function(z) Norm(z-mus[i,]))^2 } 

	x[,3]<-apply( meanz , 1, which.min)
 
	for(i in 1:K){ mus[i,]<-colSums( x[which(x[,3] == i),1:p] ) / 
		nrow( x[which(x[,3] == i),1:p]) }
	gglist[[L]]<-ggplot(x, aes(V1, V2)) + 
		geom_point(aes(colour = factor(Assignment))) +
		theme(legend.position="none")+ labs(title = 
		paste0("Iteration ", L)) +
  		geom_point(aes(x=mus[1,1], y=mus[1,2]), colour="green" , size=4)+
		geom_point(aes(x=mus[2,1], y=mus[2,2]), colour="black", size=4)
		
}
	

source("multiplot.R")

multiplot(gglist[[1]], gglist[[3]], gglist[[5]], 
	gglist[[2]], gglist[[4]], gglist[[8]], cols=2   )

 
