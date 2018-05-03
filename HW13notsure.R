U<-simult_iteration( (X)%*%t(X) ,diag(nrow(X))[,1:2])$V 

#eigen of X^t X
V<-simult_iteration( t(X)%*%X ,diag(ncol(X))[,1:2])$V 


S<-simult_iteration( t(X)%*%X ,diag(ncol(X))[,1:2])$lambda 
S<-sqrt(S)

dim(U);dim(V)

Xhat<-S[1]*U[,1]%*%t(V[,1]) + S[2]*U[,2]%*%t(V[,2] )

proj1 <- X %*% V
proj2 <- t(X) %*% U

qplot(x=proj1[,1], y=proj1[,2],   xlab="component 1", ylab="component 2") +
	geom_point(xdata=proj1[500, ], colour="red")

proj1<-as.data.frame(proj1)
tail(proj1)

ggplot(proj1,aes(V1,V2)) + geom_point( ) +  
  geom_point(data=proj1[500,], colour="red") + 
  xlim(-3.592679-.1, -3.592679+.1) + ylim(0.1736666-.1, 0.1736666+.1)

proj1[ which(proj1$V1 > -3.592679-.05 & proj1$V1 <  -3.592679+.05 & 
	proj1$V2 > 0.1736666-.05 & proj1$V2 <  0.1736666+.05) ,]



recom<-dat[c(1408,1702),]

 t(recom[,which(recom[1 , ]==1 & recom[2,] == 1)]) 

 rownames(	!%in%
rownames( t( alex[, which(alex[1,] == 1) ]) ) 

 recom[,a]
