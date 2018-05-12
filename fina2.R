rm(list = ls())
options(scipen=999)
setwd("G:\\math\\504")
#require(ggplot2,quietly=T)
source("nnfunctions.R")
nn<-read.table("nn.txt",header=T)	
X<-as.matrix(nn[,-3])
#p <- ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y))) +   
#  theme(legend.position="bottom")
#p 

#set.seed(2221)
#x=nn[,-3];y=nn[,3];eta=a;m=4
set.seed(2221)
a<- runif(22,-1,1)

head(NN(nn[,-3],theta,4))
logL(nn[,-3],nn[,3],a,4)
gradL(nn[,-3],nn[,3],a,4)
 
#start



HH<-matrix(NA,22,22)
for(i in 1:22){ HH[,i]<-Hfd(nn[,-3],nn[,3],a,4,diag(22)[,i]) }
lambda=-min(eigen(HH)$values)+.25
B<-HH+lambda*diag(22)
eigen(B)$values

x<-rep(0,22)
d<-r<- gradL(nn[,-3],nn[,3],a,4) - B%*%x
iter=0

repeat{
iter=iter+1
Alpha<-as.numeric(  (t(r)%*%r)/(t(d)%*%B%*%d) )
xnext<-x+Alpha*d
rnext<-r-Alpha*B%*%d
if( all(as.numeric( round(rnext,10) ) == 0) == T ){print(xnext)}
#store p in lsit for future BACKTRACKING
if( all(as.numeric( round(rnext,10) ) == 0) == T ){break}
Beta<- as.numeric((t(rnext)%*%rnext)/(t(r)%*%r))
dnext<-rnext+Beta*d
d<-dnext;r<-rnext;x<-xnext 
}


xnext;iter
z<-xnext


###
#lambda=32
#x<-rep(0,22)
#d<-r<- gradL(nn[,-3],nn[,3],a,4) 
#plist<-list()

#system.time(
#for(i in 1:25){
#Alpha<-as.numeric(  (t(r)%*%r)/(t(d)%*%
#	(Hfd(nn[,-3],nn[,3],a,4,d,(10^-6))  + lambda * d)) )
#xnext<-x+Alpha*d
#rnext<-r-Alpha*(Hfd(nn[,-3],nn[,3],a,4,d,epi=(10^-6) )  + lambda * d)
#if( all(as.numeric( round(rnext,10) ) == 0) == T ){print(xnext)}
#store p in lsit for future BACKTRACKING
#if( all(as.numeric( round(rnext,10) ) == 0) == T ){break}
#plist[[i]]<-xnext
#Beta<- as.numeric((t(rnext)%*%rnext)/(t(r)%*%r))
#dnext<-rnext+Beta*d
#d<-dnext;r<-rnext;x<-xnext
#})

#y=  xnext






