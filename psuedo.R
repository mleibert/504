A<-matrix(c(1,1,0,1,-1,0  ),3,2,byrow=T)
A
b<-as.matrix(rep(1,3))

VV<-eigen(t(A)%*%A)$vectors
UU<-diag(dim(A)[1])
SS<-matrix(0,dim(A)[1],dim(A)[2])
for( i in 1:dim(VV)[2]){ a<- A%*%VV[,i]; UU[,i]<-a/Norm(a); SS[i,i]<-Norm(a)}
SS<-sqrt(SS)  
rm(a)
SSS<-t(SS)
diag(SSS)<-1/diag(SS)

solve(t(A)%*%A) %*% t(A)  %*% b

solve(VV %*%t(SS)%*%t(UU)%*%UU%*%SS%*% t(VV)) %*% VV %*% t(SS) %*% t(UU) %*% b

solve(VV %*% t(SS) %*%  SS %*% t(VV)) %*% VV %*% t(SS) %*% t(UU) %*% b

solve(VV %*% t(SS) %*%  SS %*% t(VV)) %*% VV %*% t(SS) %*% t(UU) %*% b

VV %*% solve(t(SS) %*%  SS ) %*% t(VV) %*% VV %*% t(SS) %*% t(UU) %*% b

VV %*%   (SSS)  %*%  t(SSS) %*% t(VV) %*% VV %*% t(SS) %*% t(UU) %*% b

VV %*%   (SSS)  %*%  t(SSS) %*%  t(SS) %*% t(UU) %*% b
 
VV %*%  (SSS) %*% t(UU) %*% b


solve(t(SS)%*%SS) ; t(SSS)  %*%  (SSS)