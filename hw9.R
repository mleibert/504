library(readxl)
rm(list = ls())

setwd("G:\\math\\504")
life<-as.data.frame( read_excel("US Life Expectancy 2003.xls") )
 str(life)

life$survive<-0
life$die<-0
life$survive[1]<-100000
life$die[1]<-round( life$survive[1]*life[1,2] )

for(i in 2:nrow(life) ){ 
	life$survive[i]<- life$survive[i-1] - life$die[i-1]
	life$die[i]<- round( life$survive[i]*life[i,2] )	}
head(life)

forty<-life[41:nrow(life),]
rownames(forty)<-NULL

forty$p.life<-forty$survive / forty[1,3]
forty$ages<-40:(nrow(forty)+39)

 plot( forty$ages, forty$p.life )

smoothingSpline = smooth.spline(forty$ages, forty$p.life , spar=.3)
plot(forty$ages, forty$p.life, pch='+', col="red" )
lines(smoothingSpline, col="blue")



func = splinefun(x=forty$ages, y=forty$p.life, method="fmm",  ties = mean)
func(seq(40,100,1/12)) 


dat<-data.frame(1:(61*12), sort( rep(40:100,12) ) ,rep(1:12,61))
 
dat<-dat[-(722:nrow(dat)),]
names(dat)<-c("month","year","ym")
dat$p.life<-NA
dat[which(dat$ym == 1),ncol(dat)]<-forty$p.life
dat$sp.life<- func(seq(40,100,1/12)) 
dat$EPVi<-NA

 

for( i in 1:nrow(dat) ) { 
	dat$EPVi[i]<- 200 * dat$sp.life[ i] * exp( -.05 * i / 12) }
	
dat$EPV<-cumsum(dat$EPVi)


HALP<-rep(NA,1320)
for( i in 1:1320)  { 
	HALP[i]<- 200 * (func(seq(1,111,1/12))[-1])[i] * exp( -.05 * i / 12) }
cumsum(HALP)

########### Q3
 rm(list = ls())

a1<-sample(3:8,1);a2<-sample(3:8,1)
A<-matrix(sample(0:9,a1*a2,replace=T),a1,a2)
A
qr(A)

A[,2]<-A[,1]
GramSchmidt(A)
 

AA<-A[, qr(A)$pivot[seq_len(qr(A)$rank)]]



Q <- qr.Q( qr(A) )

t( Q )%*% ( Q)
t(Q[,1]) %*%Q[,2]

#https://stackoverflow.com/questions/14943422/r-independent-columns-in-matrix

GramSchmidt<-function(A){
	
	O<-A
	#get rid of linearly dependent columns
	A<-A[, qr(A)$pivot[seq_len(qr(A)$rank)]]
	
	#matrix Q
	Q<-matrix(NA,dim(A)[1],dim(A)[2])

	Q[,1] <- as.matrix(A[,1]) / norm(as.matrix(A[,1]) ,type="2")

	for( i in 2:ncol(A) ) {
		a<-as.matrix( A[,i] )
	for( j in  (i-1):1 ) {
		a<- a - (  ( t(a) %*% Q[,j]  ) *  Q[,j]  ) }
	Q[,i]<-a/norm(a,type="2") }
	
	#R gives a orthognoal matrix, need to normalize columns
	QQ<-qr.Q( qr(O) )
	for( i in 1:ncol(QQ) ) { QQ[,1] <-  QQ[,1] / 
		norm(as.matrix(QQ[,1]),type="2") }
	
	print("Our GS matrix:");print(Q)
	print("R's normalized matrix:");print(QQ)
	all( (abs(round(Q,14)) == abs(round(QQ,14))) == T)
	}


#A<-O

B<-as.matrix(qr.Q( qr(O) )[,1])

norm( B/norm(B,type="2"),type="2")

norm( B/norm(B) )
norm( B/sum(B)	 )


	
