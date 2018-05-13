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
a<-runif(22,-.1,.1)

theta<- runif(22,-.1,.1)
lambda=0

head(NN(nn[,-3],theta,4))
logL(nn[,-3],nn[,3],theta,4)
gradL(nn[,-3],nn[,3],theta,4)

#Blpha<-c(-3.7748365, 2.1178734, 0.3139542, -5.5415006, -.5958018, 
#5.8168459, 11.0561352, 0.2074085, 14.0207743, 25.2868297, 16.0428078, 
#3.8300725, 15.9792114, 23.4815794, 23.2210775, -13.5097426, -12.2073593, 
#-18.6510642, -27.0546378, -27.4979835, 15.2350215, 14.7081561)

lambda=50

#start
#system.time(
for( k in 1:500){
b<- (-gradL(nn[,-3],nn[,3],theta,4) )

x<-gradL(nn[,-3],nn[,3],theta,4) / lambda
d<-r<- b - (Hfd(nn[,-3],nn[,3],theta,4,x,(10^-2))  + lambda * x)
plist<-list()

for(i in 1:30){
#10^4 on second derivaties
hfd<-	(Hfd(nn[,-3],nn[,3],theta,4,d,(10^-2))  + lambda * d) 
Alpha<-as.numeric(  (t(r)%*%r)/(t(d)%*%	( hfd   )) )
xnext<-x+Alpha*d
rnext<-r-Alpha*( hfd  )
#if( all(as.numeric( round(rnext,10) ) == 0) == T ){print(xnext)}
#store p in lsit for future BACKTRACKING
#if( all(as.numeric( round(rnext,10) ) == 0) == T ){break}
plist[[i]]<-xnext
Beta<- as.numeric((t(rnext)%*%rnext)/(t(r)%*%r))
dnext<-rnext+Beta*d
d<-dnext;r<-rnext;x<-xnext
}

iter=iter+1
p=xnext
oldtheta=theta
theta=xnext+theta
print(logL(nn[,-3],nn[,3],theta,4))
if( logL(nn[,-3],nn[,3],theta,4) < logL(nn[,-3],nn[,3],oldtheta,4) ){
	lambda = lambda*9/10 } else { lambda = lambda*1.5 }
if( lambda < 2 ){	lambda = 10 }
if( logL(nn[,-3],nn[,3],theta,4) > logL(nn[,-3],nn[,3],oldtheta,4)  ){	
	theta=oldtheta;lambda = 100 }

print(lambda)
}

 

theta=read.csv("theta.csv")[,2]

NN(nn[,-3],theta,4)

logL(nn[,-3],nn[,3],theta,4)
logL(nn[,-3],nn[,3],a,4)

require(ggplot2)

test<-as.data.frame(	( NN(nn[,-3],theta,4 )	))
test<-cbind(test,nn[,-3])
test$y3<- ifelse( test[,1]>.5 , 0,1)
names(test)[3:4]<-c("x1","x2")

p <- ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y))) +   
  theme(legend.position="bottom")
p

ggplot(test, aes(x1, x2)) + geom_point(aes(colour = factor(y3))) +
           theme(legend.position="bottom")+ ggtitle("Test")   

p + ggtitle("Actual") ,  cols=2 ) 
