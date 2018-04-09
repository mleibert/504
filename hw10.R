rm(list = ls())
 setwd("G:\\math\\504")
options(scipen=999)

f<-function(x){exp(x)}
f(0)

 
x=0
i=-1:-20
h=10^i

#options(digits=16)
options(scipen=999)

( f(x + h) - f(x) ) / h

( f(x + h) - f(x-h) ) / (2*h)


a<- (( f(x + h) - f(x) ) / h ) 
dat<-data.frame(paste0("10^",i),a)
dat

mylist<-lapply(a, function(x)  unlist((strsplit(as.character(x), "")  )	))
mylist
digs<-rep(NA,length(i))

for(j in 1:length(i)){
	if( length(mylist[[j]])  == 1     ) {
		digs[j]<-"inaccurate"
	} else if ( mylist[[j]][1] == 1 ) { 
		digs[j]<-which(mylist[[j]] != "0")[3]-3
	} else{ digs[j]<-which(mylist[[j]] != "9")[3]-3  } }

dat$accuracy<-digs


b<- ( f(x + h) - f(x-h) ) / (2*h)
dat$b<-b
a<-b
mylist<-lapply(a, function(x)  unlist((strsplit(as.character(x), "")  )	))
mylist
digs<-rep(NA,length(i))

for(j in 1:length(i)){
	if( length(mylist[[j]])  == 1     ) {
		digs[j]<-"inaccurate"
	} else if ( mylist[[j]][1] == 0 & mylist[[j]][3] != 9 ) {
		digs[j]<-"inaccurate"
	} else if ( mylist[[j]][1] == 1 ) { 
		digs[j]<-which(mylist[[j]] != "0")[3]-3
	} else{digs[j]<-which(mylist[[j]] != "9")[3]-3  } }

dat$bccuracy<-digs
dat










###############################################################
###############################################################
###############################################################

bone<-read.table("BoneMassData.txt",header=T)
bone

plot(bone$age,bone[,4])


x=bone$age
y=bone[,4]

B<-as.matrix(x)

x4<-ifelse(x<15,0,(x-15)^3)
x5<-ifelse(x<20,0,(x-20)^3)

B<-matrix(NA,nrow(bone),6)
B[,1]<-1
B[,2]<-x
B[,3]<-x^2
B[,4]<-x^3
B[,5]<-x4
B[,6]<-x5
a<-solve(t(B)%*%B)%*%t(B)%*%as.matrix(y)

BB<-as.matrix(cbind(rep(1,nrow(bone)),x,x^2,x^3,x4,x5))
solve(t(BB)%*%BB)%*%t(BB)%*%as.matrix(y)

sx<-a[1]*1 + a[2]*x+a[3]*x^2+a[4]*x^3+a[5]*x4+a[6]*x5

X<-seq(min(x), max(x), length = 485)
X4<-ifelse(X<15,0,(X-15)^3)
X5<-ifelse(X<20,0,(X-20)^3)
sx<-a[1]*1 + a[2]*X+a[3]*X^2+a[4]*X^3+a[5]*X4+a[6]*X5
 
plot(y ~ x)
 
lines(sx ~ X,   lwd = 2, col = "blue")


#######

set.seed(1)
n <- 400
x <- 0:(n-1)/(n-1)
f <- 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
y <- f + rnorm(n, 0, sd = 2)

 x=bone$age
y=bone[,4]


mod <- lm(y ~ bs(x, knots =  c(15,20)))
pdat <- data.frame(x = seq(min(x), max(x), length = 485))
 
## predict for new `x`
pdat <- transform(pdat, yhat = predict(mod, newdata = pdat))

## now plot
ylim <- range(pdat$y, y) ## not needed, but may be if plotting CIs too


####
func = splinefun(x=x, y=sx, method="fmm", ties = mean)

par(mar=c(4.1,4.1,1,1))
smoothingSpline = smooth.spline(x, sx  )
plot(bone$age,bone[,4],pch="+")
lines(smoothingSpline, col="blue")


library(splines)
lda.pred <- lda(y ~ ns(x, knots=c(15,20)	)	)
plot(lda.pred  )

fit<-lm(y ~ bs(x,knots=c(15,20) )  )


pdat <- transform(x, yhat = predict(fit,type="response" ))
pdat <- pdat[order(pdat[,1]),]

plot(y ~ x)
lines(yhat ~ x, data = pdat, lwd = 1, col = "red")
