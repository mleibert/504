getwd()
rm(list = ls())
setwd("g:\\math\\504")
library(ggplot2)

votes<-read.table("votes_formatted.txt",stringsAsFactors=F,header=T)
tail(votes)

vote<- as.matrix(votes[,-1])

#ASSUME that all vectors are already centered
DM<-apply( t(as.matrix(votes[,-1])), 2, function(y) y - mean(y)) 

dim( cov(DM) )

N<-ncol(votes[,-1])


M<-1/N*  t( DM ) %*% DM 

m<-1/N * Reduce( '+' , lapply(
	split(vote, rep(1:ncol(vote), each = nrow(vote))), 
	function(y) y%*%t(y) ) )

m[1:5,1:5] 
M[1:5,1:5] 
99/100*cov(DM)[1:5,1:5] 

#############
#http://college.cengage.com/mathematics/larson/elementary_linear/5e/students/ch08-10/chap_10_3.pdf
 
x<-sample(1:N,dim(M)[1],replace=T)
 x<-x/sum(x)
repeat{
	y<-x
	x<- (t(M)%*%x)  / norm((t(M)%*%x))
 	if( norm(x-y) < 10^-10 ) {break} }
x

head(unitVector(x))
head(x/sqrt(sum(x^2)))
-head(eigen(M)$vectors[,1])


(eigen(M)$vectors[,1])

unitVector <- function(v) {
  vlen <- sqrt(sum(v^2))
  return ( v / vlen)
}




plot(as.numeric( t(  eigen(M)$vectors[,1] ) %*% vote[,1:ncol(vote)] ),
	rep(0, N) )



c1c2<-data.frame(
	as.numeric( t(  eigen(M)$vectors[,1] ) %*% 
	vote[,1:ncol(vote)] ),	
	as.numeric( t(  
	eigen(M)$vectors[,2] ) %*% vote[,1:ncol(vote)]) )
names(c1c2)[1:2]<-paste0("ev",1:2)

senators<-read.table("senators_formatted.txt",stringsAsFactors=T,header=T)
c1c2$party<-factor(senators$party,levels(senators$party)[ 3:1])
c1c2$zero<-0


#recall dot product: a•b = a^T b
as.numeric( t(  eigen(M)$vectors[,1] ) %*% 
	  vote[,1:ncol(vote)] ) 
 
 
c1c2$evx <- -as.numeric( t(  x ) %*% vote[,1:ncol(vote)] )

head(c1c2)
 

ggplot(c1c2, aes(x = evx,y=zero, color = party)) + 
	geom_point( size= 2, alpha = .75)

ggplot(c1c2, aes(x = ev1,y=zero, color = party)) + 
	geom_point( size= 2, alpha = .75)


ggplot(c1c2, aes(x = ev1,y=ev2, color = party)) + 
	geom_point( size= 2, alpha = .75)


 


