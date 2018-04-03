rm(list = ls())

f<-function(x){exp(x)}

fx(0)


i<-c(1,2)
10^i

x=0
i=-1:-20

h=10^i

options(digits=16)
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
	} else{digs[j]<-which(mylist[[13]] != "9")[3]-3  } }

dat$accuracy<-digs
dat



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
	} else{digs[j]<-which(mylist[[13]] != "9")[3]-3  } }

dat$bccuracy<-digs
dat





