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
theta<- runif(22,-1,1)
lambda=32

head(NN(nn[,-3],theta,4))
logL(nn[,-3],nn[,3],theta,4)
gradL(nn[,-3],nn[,3],theta,4)

Blpha<-c(-3.7748365, 2.1178734, 0.3139542, -5.5415006, -.5958018, 
5.8168459, 11.0561352, 0.2074085, 14.0207743, 25.2868297, 16.0428078, 
3.8300725, 15.9792114, 23.4815794, 23.2210775, -13.5097426, -12.2073593, 
-18.6510642, -27.0546378, -27.4979835, 15.2350215, 14.7081561)


#start



for( k in 1:50){


x<-rep(0,22)
d<-r<- gradL(nn[,-3],nn[,3],theta,4) 
plist<-list()

for(i in 1:25){
hfd<-	(Hfd(nn[,-3],nn[,3],theta,4,d,(10^-6))  + lambda * d) 
Alpha<-as.numeric(  (t(r)%*%r)/(t(d)%*%
	( hfd   )) )
xnext<-x+Alpha*d
rnext<-r-Alpha*( hfd  )
if( all(as.numeric( round(rnext,10) ) == 0) == T ){print(xnext)}
#store p in lsit for future BACKTRACKING
if( all(as.numeric( round(rnext,10) ) == 0) == T ){break}
plist[[i]]<-xnext
Beta<- as.numeric((t(rnext)%*%rnext)/(t(r)%*%r))
dnext<-rnext+Beta*d
d<-dnext;r<-rnext;x<-xnext
}

oldtheta<-theta

 xnext
for(i in 25:1){
	if( logL(nn[,-3],nn[,3],plist[[i]],4) < 
		logL(nn[,-3],nn[,3],theta,4) ) {break
	} else {next} }
theta<-plist[[i]]
i
logL(nn[,-3],nn[,3],theta,4)
logL(nn[,-3],nn[,3],oldtheta,4)

if(i <= 13 ){lambda<-lambda*1.5} else {lambda<-lambda*(2/3)}

}
logL(nn[,-3],nn[,3],theta,4)

