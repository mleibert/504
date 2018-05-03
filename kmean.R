MyKmeans<-function(x,K){
	Norm <- function(w){  sqrt(sum(w^2))}
	N<-nrow(x);	p<-ncol(x)
	x<-cbind(x,matrix(NA,N,1));names(x)[ncol(x)]<-"Assignment"
	meanz<-matrix(NA,N,p)
	x[,ncol(x)]<-sample(1:K,N,replace=T)
	mus<-matrix(NA,K,p);RAND<-sample(1:N,K);iter<-0
	for(i in 1:K){ mus[i,]<- as.numeric( x[RAND[i],1:p] ) }
	init<-RAND

	repeat{

	STOP<-x[,3];iter<-iter+1

	for( i in 1:K) { meanz[,i]<-apply(x[,1:p], 1, 
		function(z) Norm(z-mus[i,]))^2 } 

	x[,3]<-apply( meanz , 1, which.min)
 
	for(i in 1:K){ mus[i,]<-colSums( x[which(x[,3] == i),1:p] ) / 
		nrow( x[which(x[,3] == i),1:p]) }

	if( all(x[,3] == STOP) == T ) {break} }
	ASSIGNMENT<-x[,3];names(ASSIGNMENT)<-1:length(ASSIGNMENT)

	newList <- list("Cluster Means" = mus, "Cluster Vector" = ASSIGNMENT,
		"Iterations" = iter, "init" = init)
	return(newList)
	}

 

skd<-read.csv("synthetic kmeans data.csv")
MyKmeans(skd,2)

skdKM<-skd;skdKM$assigns<-MyKmeans(skd,2)[[2]]
ggplot(skdKM, aes(V1, V2)) + geom_point(aes(colour = factor(assigns )))

tmd<-read.csv("tumor microarray data.csv")
MyKmeans(tmd[-1],2)
tmdKM<-tmd
for( i in 2:5){tmdKM[,ncol(tmdKM)+1]<-MyKmeans(tmd[-1],i)[[2]] }
colnames(tmdKM)[(ncol(tmdKM)-3):(ncol(tmdKM) )]

ncol(tmdKM)
table( tmdKM[,c(1,ncol(tmdKM)-3)] )
table( tmdKM[,c(1,ncol(tmdKM)-2)] )
table( tmdKM[,c(1,ncol(tmdKM)-1)] )
table( tmdKM[,c(1,ncol(tmdKM))] )






