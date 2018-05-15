rm(list = ls())
options(scipen=999)
setwd("G:\\math\\504")
#require(ggplot2,quietly=T)
source("nnfunctions.R");source("multiplot.R")
nn<-read.table("nn.txt",header=T)	
#X<-as.matrix(nn[,-3])
#p <- ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y))) +   
#  theme(legend.position="bottom")
#p 

#set.seed(2221)
#x=nn[,-3];y=nn[,3];eta=a;m=4
set.seed(2221)
a<-runif(22,-.1,.1)

theta<- runif(22,-.1,.1)

head(NN(nn[,-3],theta,4))
logL(nn[,-3],nn[,3],theta,4)
gradL(nn[,-3],nn[,3],theta,4)

 

lambda=4

#########		start		#########
#system.time(
for( k in 1:10000){
b<- (-gradL(nn[,-3],nn[,3],theta,4) )

x<-rep(0,length(b))#gradL(nn[,-3],nn[,3],theta,4) / lambda
d<-r<- b 
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
#plist[[i]]<-xnext
Beta<- as.numeric((t(rnext)%*%rnext)/(t(r)%*%r))
dnext<-rnext+Beta*d
d<-dnext;r<-rnext;x<-xnext
}

#iter=iter+1
p=xnext
oldtheta=theta
theta=xnext+theta
print(logL(nn[,-3],nn[,3],theta,4))
if( logL(nn[,-3],nn[,3],theta,4) < logL(nn[,-3],nn[,3],oldtheta,4) ){
	lambda = lambda*9/10 } else { lambda = lambda*1.5 }
if( lambda < 10^-3 ){	lambda =  10^-2 }
if( logL(nn[,-3],nn[,3],theta,4) > logL(nn[,-3],nn[,3],oldtheta,4)  ){	
	theta=oldtheta;lambda = 1110 }

print(lambda)
} #)

 

#theta=read.csv("theta.csv")[,2]
#write.csv(theta,"theta.csv")
NN(nn[,-3],theta,4)

logL(nn[,-3],nn[,3],oldtheta,4)
logL(nn[,-3],nn[,3], theta,4)

require(ggplot2)
require(LaCroixColoR)
lacroix_palette("PassionFruit", type = "discrete")



test<-as.data.frame(	( NN(nn[,-3],theta,4 )	))
test<-cbind(test,nn[,-3])
test$y3<- ifelse( test[,1]>.5 , 0,1)
names(test)[3:4]<-c("x1","x2")
test$Y<-nn$y
test$Yd<- (test$y3+test$Y)
 
head(test)

ncol(test)

test[which(test[,7] == 1),7]<-3
test[which(test[,7] == 3), ]
test[which(test[,7] == 2), 7]<-1
test$Yd<- as.factor(test$Yd)



cols <- c("#e41a1c","#bebada","black")
 

names(cols ) <- levels(test$Yd)
colScale <- scale_colour_manual(name = "Yd",values = cols)


qq<- ggplot(test, aes(x1, x2)) + geom_point(aes(colour = factor(Yd))) +
           theme(legend.position="bottom")+ ggtitle("Test")     + colScale
qq
 


q <- ggplot(nn, aes(x1, x2)) + geom_point(aes(colour = factor(y))) +   
   theme(legend.position="bottom")

multiplot(qq,q)

x1<-matrix(4*runif(20000 )-2, 10000,2)
test<-as.data.frame((NN(x1,theta ,4)))
test<-cbind(test,x1)
test$y1<- ifelse( test[,1]>.5 , 0,1)
test$y2<- ifelse( test[,1]>.7 , 0,1)
test$y3<- ifelse( test[,1]>.3 , 0,1)
names(test)[3:4]<-c("x1","x2")

p1 <- ggplot(test, aes(x1, x2)) + geom_point(aes(colour = factor(y1))) +
 ylim(-2.5, 2.5) + xlim( -2.5,2.5 ) + ggtitle("p = 0.5") +
 theme(legend.position="bottom")








x1<-matrix(4*runif(20000 )-2, 10000,2);test<-as.data.frame((NN(x1,theta ,4)))
test<-cbind(test,x1);test$y1<- ifelse( test[,1]>.5 , 0,1)
test$y2<- ifelse( test[,1]>.7 , 0,1);test$y3<- ifelse( test[,1]>.3 , 0,1)
names(test)[3:4]<-c("x1","x2")
cols <- c("#e41a1c","#bebada");names(cols ) <- levels(test$Y1)
colScale <- scale_colour_manual(name = "Y1",values = cols)

p1 <- ggplot(test, aes(x1, x2)) + geom_point(aes(colour = factor(y1))) +
 ylim(-2.5, 2.5) + xlim( -2.5,2.5 ) + ggtitle("p = 0.5") +
 theme(legend.position="bottom") + colScale


