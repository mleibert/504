dat<-read.table("economic_data.txt",header=T)
options(scipen = 999)

y<-dat[,ncol(dat)]
y
B<-as.matrix(dat[,2:(ncol(dat)-1)])
B<-cbind(1,B)

solve(t(B) %*% B ) %*% t(B) %*% y

cor(B[,-1])
BB<-B[,-c(7,6)]
cor(BB[,-1])
solve(t(BB) %*% BB ) %*% t(BB) %*% y

#https://stats.stackexchange.com/questions/70899/
#what-correlation-makes-a-matrix-singular-and-what-
#are-implications-of-singularit

dat<-dat[,-1]
dat
( lm(B~.,data=dat) )


#####

times<-rep(NA,10)

for ( i in 1:10) {

	times[i]<-system.time( for ( j in 1:(10^i) ) { 1*1 } )[1]
	if ( times[i] > 1 ) { break}

}

system.time( for( i in 1: (1/times[7] * 10^7) ) { 1*1} )
