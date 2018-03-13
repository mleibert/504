getwd()

setwd("g:\\math\\504")

vote<-read.table("votes_formatted.txt",stringsAsFactors=F,header=T)
tail(vote)
vote<-vote[,-1]
M<-as.matrix(vote[,-1]);dim(M)
vote<-as.matrix(vote[,-1])

M<-apply(M, 2, function(y) y - mean(y))  
N<-1/ncol(M)
 
MM<-N *  (M) %*% t( M )
MM[1:3,1:3]
dim(MM)

votez<-list()
for(i in 1:ncol(M)){ votez[[i]]<- N* M[,i] %*% t(M[,i]) }
bl<-matrix(0,542,542);dim(votez[[1]])
for(i in 1:length(votez)){ bl <- bl+votez[[i]]} 
bl[1:3,1:3]